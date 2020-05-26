package co.blocke.dotty_reflection
package impl

import info._
import extractors._
import scala.quoted._
import scala.reflect._
import scala.tasty.Reflection
import scala.tasty.inspector.TastyInspector
  

class ScalaClassInspector(clazz: Class[_], initialParamMap: Map[TypeSymbol, RType]) 
    extends TastyInspector 
    with ScalaClassInspectorLike 
    with ParamGraph
    with NonCaseClassInspector:
  import Clazzes._

  var inspected: RType = UnknownInfo(clazz)

  protected def processCompilationUnit(reflect: Reflection)(root: reflect.Tree): Unit = 
    import reflect.{_, given _}
    reflect.rootContext match {
      case ctx if ctx.isJavaCompilationUnit() => inspected = JavaClassInspector.inspectClass(clazz, initialParamMap)      
      case ctx if ctx.isScala2CompilationUnit() => inspected = UnknownInfo(clazz)  // Can't do much with Scala2 classes--not Tasty
      case ctx if ctx.isAlreadyLoadedCompilationUnit() => 
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

      case t: reflect.ClassDef if t.symbol.flags.is(reflect.Flags.Enum) => // Found top-level enum (i.e. not part of a class), e.g. member of a collection
        Some(ScalaEnumInfo(className, Class.forName(className)))

      case t: reflect.ClassDef if !t.name.endsWith("$") =>

        // Get any type parameters
        val typeParams = clazz.getTypeParameters.map(_.getName.asInstanceOf[TypeSymbol]).toList

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
            val paramz = t.constructor.paramss
            val members = t.body.collect {
                case vd: reflect.ValDef => vd
              }.map(f => (f.name->f)).toMap
       
            // Find any type members matching a class type parameter
            val typeMembers = t.body.collect {
              case TypeDef(typeName, dotty.tools.dotc.ast.Trees.Ident(typeSym)) if typeParams.contains(typeSym.toString.asInstanceOf[TypeSymbol]) => 
                TypeMemberInfo(
                  typeName,
                  typeSym.toString.asInstanceOf[TypeSymbol],
                  paramMap.getOrElse(typeSym.toString.asInstanceOf[TypeSymbol],
                    TypeSymbolInfo(typeSym.toString)
                  )
                )
            }

            // Get superclass' constructor field annotations (so we can blend with this class' constructor field annotations)
            // This lazy here only is needed/eval'ed if we find an 'override' val in the parameter list of this class.
            lazy val dad = Reflector.reflectOnClass(clazz.getSuperclass)

            val fields = paramz.head.zipWithIndex.map{ (paramValDef, i) =>
              val valDef = members(paramValDef.name) // we use the members here because match types aren't resolved in paramValDef but are resolved in members
              val fieldName = valDef.name
              if(!isCaseClass)
                scala.util.Try(clazz.getDeclaredMethod(fieldName)).toOption.orElse(
                  throw new ReflectException(s"Class [$className]: Non-case class constructor arguments must all be 'val'")
                )
              val fieldAnnos = {
                val baseAnnos = 
                  if valDef.symbol.flags.is(Flags.Override) then
                    dad.asInstanceOf[ScalaClassInfo].fields.find(_.name == valDef.name).map(_.annotations).get
                  else
                    Map.empty[String,Map[String,String]]
                baseAnnos ++ paramValDef.symbol.annots.map{ a => 
                  val reflect.Apply(_, params) = a
                  val annoName = a.symbol.signature.resultSig
                  (annoName,(params collect {
                    case NamedArg(argName, Literal(Constant(argValue))) => (argName.toString, argValue.toString)
                  }).toMap)
                }.toMap
              }

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

            val classInfo = if isCaseClass then
              ScalaCaseClassInfo(className, clazz, typeParams, typeMembers, fields, annos, isValueClass)
            else
              inspectNonCaseClass(reflect, paramMap)(t, className, clazz, typeParams, typeMembers, fields, annos, isValueClass)

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