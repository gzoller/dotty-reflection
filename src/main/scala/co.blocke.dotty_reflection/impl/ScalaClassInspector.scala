package co.blocke.dotty_reflection
package impl

import model._
import extractors._
import scala.quoted._
import scala.reflect._
import scala.tasty.Reflection
import scala.tasty.inspector.TastyInspector


class ScalaClassInspector(clazz: Class[_]) extends TastyInspector:
  import Clazzes._

  var inspected: ConcreteType = UnknownInfo(clazz)

  protected def processCompilationUnit(reflect: Reflection)(root: reflect.Tree): Unit = 
    import reflect.{given _}
    reflect.rootContext match {
      case ctx if ctx.isJavaCompilationUnit() => inspected = JavaClassInspector.inspectClass(clazz)
      // TODO: May want to handle Scala2 top-level parsing of collections--we allow them as fields after all
      case ctx if ctx.isScala2CompilationUnit() => UnknownInfo(clazz)  // Can't do much with Scala2 classes--not Tasty
      case _ => inspected = inspectClass(clazz.getName, reflect)(root)
    }
    

  def inspectClass(className: String, reflect: Reflection)(tree: reflect.Tree): ConcreteType =
    import reflect.{given _}

    object Descended {
      def unapply(t: reflect.Tree): Option[ConcreteType] = descendInto(reflect)(t)
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


  private def descendInto(reflect: Reflection)(tree: reflect.Tree): Option[ConcreteType] =
    import reflect.{_, given _}
    tree match {
      case vd: reflect.ValDef if(vd.symbol.flags.is(reflect.Flags.Object)) =>
        // === Object (Scala Object) ===
        Some(ObjectInfo(vd.symbol.fullName, Class.forName(vd.symbol.fullName)))
      case t: reflect.ClassDef if !t.name.endsWith("$") =>
        val className = t.symbol.show
        val clazz = Class.forName(className)
        // cache.get(className).orElse{
        val constructor = t.constructor
        val typeParams = constructor.typeParams.map(x => x.show.stripPrefix("type ")).map(_.toString.asInstanceOf[TypeSymbol])
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

            /*
                      Reflector.reflectOnClassWithParams(clazz, tob.map(typeP => 
            inspectType(reflect)(typeP.asInstanceOf[reflect.TypeRef])
          ))*/

            /*
          implicit val ctx = reflect.rootContext.asInstanceOf[dotty.tools.dotc.core.Contexts.Context]
          val symutil = dotty.tools.dotc.transform.SymUtils(t.symbol.asInstanceOf[dotty.tools.dotc.core.Symbols.Symbol])
          val m = symutil.children //t.getClass.getMethods.toList.mkString("   \n")
          println("M: "+m.map(_.fullName))
          */
        else
          // === Scala Class (case or non-case) ===
          val isCaseClass = t.symbol.flags.is(reflect.Flags.Case)
          val paramz = constructor.paramss
          val members = t.body.collect {
            case vd: reflect.ValDef => vd
          }.map(f => (f.name->f)).toMap
          val fields = paramz.head.zipWithIndex.map{ (paramValDef, i) =>
            val valDef = members(paramValDef.name) // we use the members here because match types aren't resolved in paramValDef but are resolved in members
            val fieldName = valDef.name
            if(!isCaseClass)
              scala.util.Try(clazz.getDeclaredMethod(fieldName)).toOption.orElse(
                throw new Exception(s"Class [$className]: Non-case class constructor arguments must all be 'val'")
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

          ScalaClassInfo(className, clazz, fields, typeParams, annos, isValueClass)
        Some(inspected)
        // }
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
      .getOrElse(throw new Exception(s"Problem with class $className, field ${valDef.name}: All non-case class constructor fields must be vals"))
    ScalaFieldInfo(index, valDef.name, fieldTypeInfo, annos, valueAccessor, defaultAccessor)


  def inspectType(reflect: Reflection)(typeRef: reflect.TypeRef): ALL_TYPE = 
    import reflect.{_, given _}

    val classSymbol = typeRef.classSymbol.get
    // Handle gobbled non-class scala.Enumeration.Value (old 2.x Enumeration class values)
    val (is2xEnumeration, className) = classSymbol.fullName match { 
      case raw if raw == "scala.Enumeration.Value" => 
        val emerationClass = typeRef.typeSymbol.fullName
        (true, emerationClass.dropRight(emerationClass.length - emerationClass.lastIndexOf('$')))
      case _                                       => (false, classSymbol.fullName)
    }

    typeRef match {
      // Scala3 opaque type alias
      //----------------------------------------
      case named: dotty.tools.dotc.core.Types.NamedType if typeRef.isOpaqueAlias =>
        inspectType(reflect)(typeRef.translucentSuperType.asInstanceOf[reflect.TypeRef]) match {
          case c:ConcreteType => AliasInfo(typeRef.show, c)
          case _ => throw new Exception("Opaque aliases for type symbols currently unsupported")
        }

      // Scala3 Tasty-equipped type incl. primitive types
      //----------------------------------------
      case named: dotty.tools.dotc.core.Types.NamedType => 
        val isTypeParam = typeRef.typeSymbol.flags.is(Flags.Param)   // Is 'T' or a "real" type?  (true if T)
        val anySymbol = Symbol.classSymbol("scala.Any")
        classSymbol match {
          case cs if cs == anySymbol && !isTypeParam => PrimitiveType.Scala_Any
          case cs if cs == anySymbol => typeRef.name.asInstanceOf[TypeSymbol]  // TypeSymbols Foo[T] have typeRef of Any
          case _ =>
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
              case _ =>
                if(!isTypeParam) 
                  descendInto(reflect)(classSymbol.tree).get
                else
                  typeRef.name.asInstanceOf[TypeSymbol]
            }
        }

      // Union Type
      //----------------------------------------
      case OrType(left,right) =>
        val resolvedLeft: ALL_TYPE = inspectType(reflect)(left.asInstanceOf[reflect.TypeRef])
        val resolvedRight: ALL_TYPE = inspectType(reflect)(right.asInstanceOf[reflect.TypeRef])
        StaticUnionInfo("__union_type__", List.empty[TypeSymbol], resolvedLeft, resolvedRight)

      // Intersection Type
      //----------------------------------------
      case AndType(left,right) =>
        val resolvedLeft: ALL_TYPE = inspectType(reflect)(left.asInstanceOf[reflect.TypeRef])
        val resolvedRight: ALL_TYPE = inspectType(reflect)(right.asInstanceOf[reflect.TypeRef])
        StaticIntersectionInfo("__intersection_type__", List.empty[TypeSymbol], resolvedLeft, resolvedRight)
    
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