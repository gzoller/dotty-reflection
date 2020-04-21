package co.blocke.dotty_reflection
package impl

import info._
import extractors._
import scala.quoted._
import scala.reflect._
import scala.tasty.Reflection
import scala.tasty.inspector.TastyInspector


class ScalaClassInspector(clazz: Class[_], initialParamMap: Map[TypeSymbol, RType]) extends TastyInspector with ParamGraph:
  import Clazzes._

  var inspected: RType = UnknownInfo(clazz)

  protected def processCompilationUnit(reflect: Reflection)(root: reflect.Tree): Unit = 
    import reflect.{_, given _}
    reflect.rootContext match {
      case ctx if ctx.isJavaCompilationUnit() => inspected = JavaClassInspector.inspectClass(clazz, initialParamMap)      
      case ctx if ctx.isScala2CompilationUnit() => inspected = UnknownInfo(clazz)  // Can't do much with Scala2 classes--not Tasty
      case ctx if ctx.isAlreadyLoadedCompilationUnit() => 
        val clazz = Class.forName(ctx.compilationUnitClassname())
        ExtractorRegistry.extractors.collectFirst {
          case e if e.matches(clazz) => inspected = e.emptyInfo(clazz, initialParamMap)
        }
      case _ => inspected = inspectClass(clazz.getName, reflect, initialParamMap)(root)
    }
    

  def inspectClass(className: String, reflect: Reflection, paramMap: Map[TypeSymbol,RType])(tree: reflect.Tree): RType =
    import reflect.{given _}

    object Descended {
      def unapply(t: reflect.Tree): Option[RType] = descendInto(className, reflect, paramMap)(t)
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
    foundClass.getOrElse( UnknownInfo(Class.forName(className)) )


  private def descendInto(className: String, reflect: Reflection, paramMap: Map[TypeSymbol,RType])(tree: reflect.Tree): Option[RType] =
    import reflect.{_, given _}
    tree match {

      case pkg: reflect.PackageClause => // nested packages
        Some(inspectClass(className, reflect, paramMap)(tree))

      case vd: reflect.ValDef if(vd.symbol.flags.is(reflect.Flags.Object)) =>
        // === Object (Scala Object) ===
        Some( ObjectInfo(vd.symbol.fullName, Class.forName(vd.symbol.fullName)) )

      case t: reflect.ClassDef if !t.name.endsWith("$") =>

        val clazz = Class.forName(className)
        val constructor = t.constructor

        // Get any type parameters
        val typeParams = constructor.typeParams.map( _ match {
          case TypeDef(tpeSym,_) => tpeSym.asInstanceOf[TypeSymbol]
        })

        val inspected: RType =
          if(t.symbol.flags.is(reflect.Flags.Trait)) then
            // === Trait ===
            if t.symbol.flags.is(reflect.Flags.Sealed) then
              SealedTraitInfo(
                className, 
                clazz, 
                typeParams, 
                t.symbol.children.map(c => Reflector.reflectOnClass(Class.forName(c.fullName))))
            else
              val actualTypeParams = typeParams.map(_ match {
                case p if paramMap.contains(p) => paramMap(p)
                case p => TypeSymbolInfo(p.asInstanceOf[String])
              })
              val traitInfo = TraitInfo(className, clazz, typeParams, actualTypeParams)

              // Now figure out type parameter graph
              registerParents(reflect)(t, traitInfo)

              traitInfo

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
                TypeMemberInfo(
                  typeName,
                  paramMap.getOrElse(typeSym.toString.asInstanceOf[TypeSymbol],
                    TypeSymbolInfo(typeSym.toString)
                  )
                )
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

              inspectField(reflect, paramMap)(valDef, i, fieldAnnos, className) 
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

            val classInfo = ScalaClassInfo(className, clazz, typeParams, typeMembers, fields, annos, isValueClass)

            // Now figure out type parameter graph
            registerParents(reflect)(t, classInfo)

            classInfo

        
        Some(inspected)

      case _ =>
        None
    }


  private def inspectField(reflect: Reflection, paramMap: Map[TypeSymbol,RType])(
      valDef: reflect.ValDef, 
      index: Int, 
      annos: Map[String,Map[String,String]], 
      className: String
    ): FieldInfo =

    import reflect.{_, given _}

    val fieldType: RType = inspectType(reflect, paramMap)(valDef.tpt.tpe.asInstanceOf[reflect.TypeRef])

    // See if there's default values specified -- look for gonzo method on companion class.  If exists, default value is available.
    val defaultAccessor = 
      fieldType match {
        case _: TypeSymbolInfo => None
        case _ =>
          scala.util.Try{
            val companionClazz = Class.forName(className+"$") // This will fail for non-case classes, including Java classes
            val defaultMethod = companionClazz.getMethod("$lessinit$greater$default$"+(index+1)) // This will fail if there's no default value for this field
            val const = companionClazz.getDeclaredConstructor()
            const.setAccessible(true)
            ()=>defaultMethod.invoke(const.newInstance())
          }.toOption
      }

    val clazz = Class.forName(className)
    val valueAccessor = scala.util.Try(clazz.getDeclaredMethod(valDef.name))
      .getOrElse(throw new ReflectException(s"Problem with class $className, field ${valDef.name}: All non-case class constructor fields must be vals"))


    // Figure out the original type symbols, i.e. T, (if any)
    val valTypeRef = valDef.tpt.tpe.asInstanceOf[reflect.TypeRef]
    val isTypeParam = valTypeRef.typeSymbol.flags.is(Flags.Param)
    val originalTypeSymbol = if isTypeParam then Some(valTypeRef.name.asInstanceOf[TypeSymbol]) else None
  
    ScalaFieldInfo(index, valDef.name, fieldType, annos, valueAccessor, defaultAccessor, originalTypeSymbol)


  def inspectType(reflect: Reflection, paramMap: Map[TypeSymbol,RType])(typeRef: reflect.TypeRef): RType = 
    import reflect.{_, given _}

    typeRef.classSymbol match {

      // Intersection types don't have a class symbol, so don't assume one!
      case None =>
        typeRef match {
          // Intersection Type
          //----------------------------------------
          case AndType(left,right) =>
            val resolvedLeft: RType = inspectType(reflect, paramMap)(left.asInstanceOf[reflect.TypeRef])
            val resolvedRight: RType = inspectType(reflect, paramMap)(right.asInstanceOf[reflect.TypeRef])
            IntersectionInfo(INTERSECTION_CLASS, resolvedLeft, resolvedRight)
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
            inspectType(reflect, paramMap)(typeRef.translucentSuperType.asInstanceOf[reflect.TypeRef]) match {
              case t: TypeSymbolInfo => throw new ReflectException("Opaque aliases for type symbols currently unsupported")
              case t => AliasInfo(typeRef.show, t)
            }

          // Scala3 Tasty-equipped type incl. primitive types
          //----------------------------------------
          case named: dotty.tools.dotc.core.Types.NamedType => 
            val isTypeParam = typeRef.typeSymbol.flags.is(Flags.Param)   // Is 'T' or a "real" type?  (true if T)
            val anySymbol = Symbol.classSymbol("scala.Any")
            classSymbol match {
              case cs if isTypeParam     => 
                // See if we can resolve the type symbol
                paramMap.get(typeRef.name.asInstanceOf[TypeSymbol]).getOrElse(
                  TypeSymbolInfo(typeRef.name)  // TypeSymbols Foo[T] have typeRef of Any
                  )
              case cs if cs == anySymbol => PrimitiveType.Scala_Any
              case cs =>
                Class.forName(className) match {
                  case c if c <:< EnumClazz => ScalaEnumInfo(className, c)
                  case c if is2xEnumeration => ScalaEnumerationInfo(className, c)
                  case c                    => Reflector.reflectOnClass(c)  // it's some other class, likely a Java or 2.x Scala class
                }
            }

          // Union Type
          //----------------------------------------
          case OrType(left,right) =>
            val resolvedLeft = inspectType(reflect, paramMap)(left.asInstanceOf[reflect.TypeRef])
            val resolvedRight = inspectType(reflect, paramMap)(right.asInstanceOf[reflect.TypeRef])
            UnionInfo(UNION_CLASS, resolvedLeft, resolvedRight)
        
          // Most other "normal" Types
          //----------------------------------------
          case AppliedType(t,tob) => 
            val clazz = Class.forName(className)

            val foundType: Option[RType] = ExtractorRegistry.extractors.collectFirst {
              case e if e.matches(clazz) => e.extractInfo(reflect, paramMap)(t, tob, className, clazz, this)   
            }
            foundType.getOrElse{
              // Some other class we need to descend into, including a parameterized Scala class
              Reflector.reflectOnClassWithParams(clazz, tob.map(typeP => 
                inspectType(reflect, paramMap)(typeP.asInstanceOf[reflect.TypeRef])
              ))
            }
        
          case x => 
            UnknownInfo(Class.forName(className))
        }
    }