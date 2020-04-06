package co.blocke.dotty_reflection
package impl

import infos._
import extractors._
import scala.quoted._
import scala.reflect._
import scala.tasty.Reflection
import scala.tasty.inspector.TastyInspector


class ScalaClassInspector(clazz: Class[_]) extends TastyInspector:
  import Clazzes._

  var inspected: ConcreteType = UnknownInfo(clazz)

  protected def processCompilationUnit(reflect: Reflection)(root: reflect.Tree): Unit = 
    import reflect.{_, given _}
    reflect.rootContext match {
      case ctx if ctx.isJavaCompilationUnit() => inspected = JavaClassInspector.inspectClass(clazz)      
      case ctx if ctx.isScala2CompilationUnit() => UnknownInfo(clazz)  // Can't do much with Scala2 classes--not Tasty
      case ctx if ctx.isAlreadyLoadedCompilationUnit() => 
        val clazz = Class.forName(ctx.compilationUnitClassname())
        ExtractorRegistry.extractors.collectFirst {
          case e if e.matches(clazz) => inspected = e.emptyInfo(clazz)
        }
      case _ => inspected = inspectClass(clazz.getName, reflect)(root)
    }
    

  def inspectClass(className: String, reflect: Reflection)(tree: reflect.Tree): ConcreteType =
    import reflect.{given _}

    object Descended {
      def unapply(t: reflect.Tree): Option[ConcreteType] = descendInto(className, reflect)(t)
    }
  
    // We expect a certain structure:  PackageClause, which contains ClassDef's for target class + companion object
    val foundClass = tree match {
      case t: reflect.PackageClause => 
        t.stats collectFirst {
          case Descended(m) => m
        }
      case t => 
        None  // not what we expected!
    }
    foundClass.getOrElse(UnknownInfo(Class.forName(className)))


  private def descendInto(className: String, reflect: Reflection)(tree: reflect.Tree): Option[ConcreteType] =
    import reflect.{_, given _}
    tree match {

      case pkg: reflect.PackageClause => // nested packages
        Some(inspectClass(className, reflect)(tree))

      case vd: reflect.ValDef if(vd.symbol.flags.is(reflect.Flags.Object)) =>
        // === Object (Scala Object) ===
        Some(ObjectInfo(vd.symbol.fullName, Class.forName(vd.symbol.fullName)))

      case t: reflect.ClassDef if !t.name.endsWith("$") =>
        val clazz = Class.forName(className)
        val constructor = t.constructor

        // Get any type parameters
        val typeParams = constructor.typeParams.map( _ match {
          case TypeDef(tpeSym,_) => tpeSym.asInstanceOf[TypeSymbol]
        })

        val inspected: ConcreteType = if(t.symbol.flags.is(reflect.Flags.Trait)) then
          // === Trait ===
          if t.symbol.flags.is(reflect.Flags.Sealed) then
            SealedTraitInfo(
              className, 
              clazz, 
              typeParams, 
              t.symbol.children.map(c => Reflector.reflectOnClass(Class.forName(c.fullName))))
          else
            TraitInfo(className, clazz, typeParams, Nil)
        else
          // === Scala Class (case or non-case) ===
          val isCaseClass = t.symbol.flags.is(reflect.Flags.Case)
          val paramz = constructor.paramss
          val members = t.body.collect {
            case vd: reflect.ValDef => vd
          }.map(f => (f.name->f)).toMap

          // Find any type members matching a class type parameter
          val typeMembers = t.body.collect {
            case TypeDef(typeName, dotty.tools.dotc.ast.Trees.Ident(typeSym)) if typeParams.contains(typeSym.toString.asInstanceOf[TypeSymbol]) => 
              TypeMember(typeName, typeSym.toString.asInstanceOf[TypeSymbol], PrimitiveType.Scala_Any) // Any is a placeholder, to be replaced by sewTypeParams()
          }

          val fields = paramz.head.zipWithIndex.map{ (paramValDef, i) =>
            val valDef = members(paramValDef.name) // we use the members here because match types aren't resolved in paramValDef but are resolved in members
            val fieldName = valDef.name
            if(!isCaseClass)
              scala.util.Try(clazz.getDeclaredMethod(fieldName)).toOption.orElse(
                throw new ReflectException(s"Class [$className]: Non-case class constructor arguments must all be 'val'")
              )
            // Field annotations (stored internal 'val' definitions in class)
            val annoSymbol = members.get(fieldName).get.symbol.annots.filter( a => !a.symbol.signature.resultSig.startsWith("scala.annotation.internal."))
            val fieldAnnos = annoSymbol.map{ a => 
              val reflect.Apply(_, params) = a
              val annoName = a.symbol.signature.resultSig
              (annoName,(params collect {
                case NamedArg(argName, Literal(Constant(argValue))) => (argName.toString, argValue.toString)
              }).toMap)
            }.toMap

            inspectField(reflect)(valDef, i, fieldAnnos, className) 
          }

          // Class annotations
          val annoSymbol = t.symbol.annots.filter( a => !a.symbol.signature.resultSig.startsWith("scala.annotation.internal."))
          val annos = annoSymbol.map{ a => 
            val reflect.Apply(_, params) = a
            val annoName = a.symbol.signature.resultSig
            (annoName,(params collect {
              case NamedArg(argName, Literal(Constant(argValue))) => (argName.toString, argValue.toString)
            }).toMap)
          }.toMap

          val isValueClass = t.parents.collectFirst {
            case Apply(Select(New(x),_),_) => x 
          }.map(_.symbol.name == "AnyVal").getOrElse(false)

          ScalaClassInfo(className, clazz, typeMembers, fields, typeParams, annos, isValueClass)

        Some(inspected)

      case _ =>
        None
    }


  private def inspectField(reflect: Reflection)(
    valDef: reflect.ValDef, 
    index: Int, 
    annos: Map[String,Map[String,String]], 
    className: String
    ): FieldInfo =
    import reflect.{_, given _}

    val fieldTypeInfo: ALL_TYPE = inspectType(reflect)(valDef.tpt.tpe.asInstanceOf[reflect.TypeRef])

    // See if there's default values specified -- look for gonzo method on companion class.  If exists, default value is available.
    val defaultAccessor = fieldTypeInfo match
      case _: TypeSymbol => None
      case _ =>
        scala.util.Try{
          val companionClazz = Class.forName(className+"$") // This will fail for non-case classes, including Java classes
          val defaultMethod = companionClazz.getMethod("$lessinit$greater$default$"+(index+1)) // This will fail if there's no default value for this field
          val const = companionClazz.getDeclaredConstructor()
          const.setAccessible(true)
          ()=>defaultMethod.invoke(const.newInstance())
        }.toOption

    val clazz = Class.forName(className)
    val valueAccessor = scala.util.Try(clazz.getDeclaredMethod(valDef.name))
      .getOrElse(throw new ReflectException(s"Problem with class $className, field ${valDef.name}: All non-case class constructor fields must be vals"))
    ScalaFieldInfo(index, valDef.name, fieldTypeInfo, annos, valueAccessor, defaultAccessor)


  def inspectType(reflect: Reflection)(typeRef: reflect.TypeRef): ALL_TYPE = 
    import reflect.{_, given _}

    typeRef.classSymbol match {

      // Intersection types don't have a class symbol, so don't assume one!
      case None =>
        typeRef match {
          // Intersection Type
          //----------------------------------------
          case AndType(left,right) =>
            val resolvedLeft: ALL_TYPE = inspectType(reflect)(left.asInstanceOf[reflect.TypeRef])
            val resolvedRight: ALL_TYPE = inspectType(reflect)(right.asInstanceOf[reflect.TypeRef])
            IntersectionInfo(Reflector.INTERSECTION_CLASS, resolvedLeft, resolvedRight)
          case u => throw new ReflectException("Unsupported TypeRef: "+typeRef)
        }

      case Some(classSymbol) =>
        // Handle gobbled non-class scala.Enumeration.Value (old 2.x Enumeration class values)
        val ENUM_CLASSNAME = "scala.Enumeration.Value"
        val (is2xEnumeration, className) = classSymbol.fullName match { 
          case raw if raw == ENUM_CLASSNAME => 
            val enumerationClass = typeRef.typeSymbol.fullName
            if( enumerationClass == ENUM_CLASSNAME ) then
              // If caller did NOT defined a type member (type X = Value) inside their Enumeration class
              val enumClassName = typeRef.qualifier.asInstanceOf[reflect.TermRef].termSymbol.moduleClass.fullName.dropRight(1) // chop the '$' off the end!
              (true, enumClassName)
            else
              // If caller defined a type member (type X = Value) inside their Enumeration class
              (true, enumerationClass.dropRight(enumerationClass.length - enumerationClass.lastIndexOf('$')))
          case _  => (false, classSymbol.fullName)
        }

        typeRef match {
          // Scala3 opaque type alias
          //----------------------------------------
          case named: dotty.tools.dotc.core.Types.NamedType if typeRef.isOpaqueAlias =>
            inspectType(reflect)(typeRef.translucentSuperType.asInstanceOf[reflect.TypeRef]) match {
              case c:ConcreteType => AliasInfo(typeRef.show, c)
              case _ => throw new ReflectException("Opaque aliases for type symbols currently unsupported")
            }

          // Scala3 Tasty-equipped type incl. primitive types
          //----------------------------------------
          case named: dotty.tools.dotc.core.Types.NamedType => 
            val isTypeParam = typeRef.typeSymbol.flags.is(Flags.Param)   // Is 'T' or a "real" type?  (true if T)
            val anySymbol = Symbol.classSymbol("scala.Any")
            classSymbol match {
              case cs if isTypeParam => typeRef.name.asInstanceOf[TypeSymbol]  // TypeSymbols Foo[T] have typeRef of Any
              case cs if cs == anySymbol => 
                PrimitiveType.Scala_Any
              case cs =>
                Class.forName(className) match {
                  case c if c =:= BooleanClazz     => PrimitiveType.Scala_Boolean
                  case c if c =:= ByteClazz        => PrimitiveType.Scala_Byte
                  case c if c =:= CharClazz        => PrimitiveType.Scala_Char
                  case c if c =:= DoubleClazz      => PrimitiveType.Scala_Double
                  case c if c =:= FloatClazz       => PrimitiveType.Scala_Float
                  case c if c =:= IntClazz         => PrimitiveType.Scala_Int
                  case c if c =:= LongClazz        => PrimitiveType.Scala_Long
                  case c if c =:= ShortClazz       => PrimitiveType.Scala_Short
                  case c if c =:= StringClazz      => PrimitiveType.Scala_String
                  case c if c <:< EnumClazz        => ScalaEnum(className, c)
                  case c if is2xEnumeration        => ScalaEnumeration(className, c)
                  case c =>
                    if(isTypeParam) then
                      typeRef.name.asInstanceOf[TypeSymbol] // it's a type symbol, T
                    else
                      Reflector.reflectOnClass(c)  // it's some other class, likely a Java or 2.x Scala class
                }
            }

          // Union Type
          //----------------------------------------
          case OrType(left,right) =>
            val resolvedLeft: ALL_TYPE = inspectType(reflect)(left.asInstanceOf[reflect.TypeRef])
            val resolvedRight: ALL_TYPE = inspectType(reflect)(right.asInstanceOf[reflect.TypeRef])
            UnionInfo(Reflector.UNION_CLASS, resolvedLeft, resolvedRight)
        
          // Most other "normal" Types
          //----------------------------------------
          case AppliedType(t,tob) => 
            val clazz = Class.forName(className)

            val foundType: Option[ALL_TYPE] = ExtractorRegistry.extractors.collectFirst {
              case e if e.matches(clazz) => e.extractInfo(reflect)(t, tob, className, clazz, this)   
            }
            foundType.getOrElse{
              // Some other class we need to descend into, including a parameterized Scala class
              Reflector.reflectOnClassWithParams(clazz, tob.map(typeP => 
                inspectType(reflect)(typeP.asInstanceOf[reflect.TypeRef])
              ))
            }

                // TODO in DottyJack: Here's how to get companion object to then find newBuilder method to construct the List-like thing
                // val companionClazz = Class.forName(className+"$").getMethod("newBuilder")
                // println("HERE "+companionClazz)
          
          case x => 
            UnknownInfo(Class.forName(className))
        }
    }