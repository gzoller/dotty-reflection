package co.blocke.dotty_reflection
package info

import impl._

trait ClassInfo extends RType: 
  val name:                       String
  lazy val fields:                Array[FieldInfo]
  lazy val typeMembers:           Array[TypeMemberInfo]
  lazy val annotations:           Map[String, Map[String,String]]
  lazy val mixins:                List[String]

  def hasMixin(mixin: String): Boolean = mixins.contains(mixin)


abstract class ScalaClassInfoBase protected[dotty_reflection] (
    name:                   String,
    _typeMembers:           Array[TypeMemberInfo],
    _fields:                Array[FieldInfo],
    _annotations:           Map[String, Map[String,String]],
    _mixins:                List[String],
    isValueClass:           Boolean
  ) extends ClassInfo:

  // All this laziness is because Java classes use a proxy that isn't resolved until runtime.
  lazy val typeMembers = _typeMembers
  lazy val annotations = _annotations
  lazy val mixins = _mixins
  lazy val infoClass: Class[_] = Class.forName(name)
  lazy val typeParams = infoClass.getTypeParameters.toList.map(_.getName.asInstanceOf[TypeSymbol])

  override def toType(reflect: scala.tasty.Reflection): reflect.Type = 
    import reflect.{_, given _}
    val actualParameterTypes = fields.collect{
      case f if f.originalSymbol.isDefined => f.originalSymbol.get -> f.fieldType.toType(reflect).asInstanceOf[Type]
    }.toMap
    if typeParams.nonEmpty then
      val args = typeParams.map(sym => actualParameterTypes.getOrElse(sym,PrimitiveType.Scala_Any.toType(reflect).asInstanceOf[Type])).toList
      AppliedType(Type(infoClass), args)
    else
      reflect.Type(infoClass)
 
  // Fields may be self-referencing, so we need to unwind this...
  lazy val fields = _fields.map( f => f.fieldType match {
    case s: SelfRefRType => f.asInstanceOf[ScalaFieldInfo].copy(fieldType = s.resolve)
    case s => f
  })

  lazy val constructor = infoClass.getConstructor(fields.map(_.asInstanceOf[ScalaFieldInfo].constructorClass):_*)

  override def findPaths(findSyms: Map[TypeSymbol,Path], referenceTrait: Option[TraitInfo] = None): (Map[TypeSymbol, Path], Map[TypeSymbol, Path]) = 
    val interestingFields = referenceTrait.map{ refTrait =>
       fields.filter(f => refTrait.fields.map(_.name).contains(f.name))
    }.getOrElse(fields)
    interestingFields.foldLeft((Map.empty[TypeSymbol,Path], findSyms)) { (acc, f) =>
      val (found, notFound) = acc
      val nameForPath = referenceTrait.map(_.name).getOrElse(name)
      if notFound.nonEmpty then
        val pathElement = 
          if referenceTrait.isDefined then
            TraitPathElement(nameForPath, f.name)
          else
            ClassPathElement(nameForPath, f.name)
        f.fieldType match {
          case ts: TypeSymbolInfo if notFound.contains(ts.name.asInstanceOf[TypeSymbol]) =>
            // This field's type is one of the sought-after TypeSymbols...
            val sym = ts.name.asInstanceOf[TypeSymbol]
            (found + (sym -> notFound(sym).push(pathElement)), notFound - sym)
          case _ =>
            // Or it's not...
            val (themThatsFound, themThatsStillLost) = f.fieldType.findPaths(notFound.map( (k,v) => k -> v.push(pathElement) ))
            (found ++ themThatsFound, themThatsStillLost.map( (k,v) => k -> findSyms(k) ))
        }
      else
        (found, notFound)
      }

  // def _copy( newFields: Array[FieldInfo] ): ScalaClassInfoBase

  // override def resolveTypeParams( paramMap: Map[TypeSymbol, RType] ): RType = 
  //   println("-------> RESOLVING!")
  //   val newFields = fields.map{ f =>
  //     f.fieldType match {
  //       case ts: TypeSymbolInfo if paramMap.contains(ts.name.asInstanceOf[TypeSymbol]) => f.asInstanceOf[ScalaFieldInfo].copy(fieldType = paramMap(ts.name.asInstanceOf[TypeSymbol]))
  //       case pt: impl.PrimitiveType => f
  //       case other => f.asInstanceOf[ScalaFieldInfo].copy(fieldType = other.resolveTypeParams(paramMap))
  //     }
  //   }
  //   println(s"After $name: "+newFields.toList)
  //   this._copy(newFields)

  // Used for ScalaJack writing of type members ("external type hints").  If some type members are not class/trait, it messes up any
  // type hint modifiers, so for the purposes of serialization we want to filter out "uninteresting" type members (e.g. primitives)
  def filterTraitTypeParams: ScalaClassInfoBase 

  def show(tab:Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    if seenBefore.contains(name) then
      {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName + s"($name) (self-ref recursion)\n"
    else
      {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName 
      + {if isValueClass then "--Value Class--" else ""}
      + s"($name):\n"
      + tabs(newTab) + "fields:\n" + {if modified then fields.map(f => tabs(newTab+1) + f.name+s"<${f.fieldType.infoClass.getName}>\n").mkString else fields.map(_.show(newTab+1, name::seenBefore)).mkString}
      + {if annotations.nonEmpty then tabs(newTab) + "annotations: "+annotations.toString + "\n" else ""}
      + {if( typeMembers.nonEmpty ) tabs(newTab) + "type members:\n" + typeMembers.map(_.show(newTab+1,name :: seenBefore)).mkString else ""}

//------------------------------------------------------------

case class ScalaCaseClassInfo protected[dotty_reflection] (
    name:                   String,
    _typeMembers:           Array[TypeMemberInfo],
    _fields:                Array[FieldInfo],
    _annotations:           Map[String, Map[String,String]],
    _mixins:                List[String],
    isValueClass:           Boolean
  ) extends ScalaClassInfoBase(name, _typeMembers, _fields, _annotations, _mixins, isValueClass):

  // Used for ScalaJack writing of type members ("external type hints").  If some type members are not class/trait, it messes up any
  // type hint modifiers, so for the purposes of serialization we want to filter out "uninteresting" type members (e.g. primitives)
  def filterTraitTypeParams: ScalaClassInfoBase = this.copy( _typeMembers = typeMembers.filter(tm => tm.memberType.isInstanceOf[TraitInfo] || tm.memberType.isInstanceOf[ScalaCaseClassInfo]) )

  // def _copy( newFields: Array[FieldInfo] ): ScalaClassInfoBase = this.copy(_fields = newFields)


//------------------------------------------------------------


case class ScalaClassInfo protected[dotty_reflection] (
    name:                   String,
    _typeMembers:           Array[TypeMemberInfo],
    _fields:                Array[FieldInfo],  // constructor fields
    nonConstructorFields:   Array[ScalaFieldInfo],
    _annotations:           Map[String, Map[String,String]],
    _mixins:                List[String],
    isValueClass:           Boolean
  ) extends ScalaClassInfoBase(name, _typeMembers, _fields, _annotations, _mixins, isValueClass):

  // Used for ScalaJack writing of type members ("external type hints").  If some type members are not class/trait, it messes up any
  // type hint modifiers, so for the purposes of serialization we want to filter out "uninteresting" type members (e.g. primitives)
  def filterTraitTypeParams: ScalaClassInfoBase = this.copy( _typeMembers = typeMembers.filter(tm => tm.memberType.isInstanceOf[TraitInfo] || tm.memberType.isInstanceOf[ScalaCaseClassInfo]) )
  // def _copy( newFields: Array[FieldInfo] ): ScalaClassInfoBase = this.copy(_fields = newFields)

  override def show(tab:Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    val showNCFields = {if !modified then nonConstructorFields else nonConstructorFields.sortBy(_.name) }

    if seenBefore.contains(name) then
      {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName + s"($name) (self-ref recursion)\n"
    else
      {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName 
      + {if isValueClass then "--Value Class--" else ""}
      + s"($name):\n"
      + tabs(newTab) + "fields:\n" + fields.map(_.show(newTab+1,name :: seenBefore)).mkString
      + tabs(newTab) + "non-constructor fields:\n" + showNCFields.map(_.show(newTab+1,name :: seenBefore, supressIndent, modified)).mkString
      + {if annotations.nonEmpty then tabs(newTab) + "annotations: "+annotations.toString + "\n" else ""}
      + {if( typeMembers.nonEmpty ) tabs(newTab) + "type members:\n" + typeMembers.map(_.show(newTab+1,name :: seenBefore)).mkString else ""}


//------------------------------------------------------------


/** Java class reflection has a special problem... we need the class file, which isn't available during compilation (i.e. inside a macro).
 *  The best we can do is capture the name of the class and materialize/reflect on the class outside of the macro, lazy-like.
 */
/* TODO
case class JavaClassInfo protected[dotty_reflection] ( name: String, paramMap: TypeSymbolMap ) extends ClassInfo:
  lazy val infoClass: Class[_] = Class.forName(name)
  private lazy val proxy = impl.JavaClassInspector.inspectJavaClass(infoClass, paramMap, true).asInstanceOf[JavaClassInfoProxy]
  lazy val fields = proxy.fields
  lazy val typeMembers:           Array[TypeMemberInfo]           = proxy.typeMembers
  lazy val annotations:           Map[String, Map[String,String]] = proxy.annotations
  lazy val mixins:                List[String]                    = proxy.mixins

  def field(name: String): Option[JavaFieldInfo] = fieldsByName.get(name)

  private lazy val fieldsByName = fields.map(f => (f.name, f.asInstanceOf[JavaFieldInfo])).toMap

  def show(tab:Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}

    if seenBefore.contains(name) then
      {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName + s"($name) (self-ref recursion)\n"
    else
      {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName 
      + s"($name):\n"
      + tabs(newTab) + "fields:\n" + fields.map(_.show(newTab+1,name :: seenBefore)).mkString
      + {if annotations.nonEmpty then tabs(newTab) + "annotations: "+annotations.toString + "\n" else ""}
  

case class JavaClassInfoProxy protected[dotty_reflection] (
    name:                   String,
    _fields:                Array[FieldInfo],
    _annotations:           Map[String, Map[String,String]]
  ) extends RType:

  lazy val annotations = _annotations
  lazy val infoClass: Class[_] = Class.forName(name)

  // Run up the interitance tree to the top (Object) to get all the superclasses and mixin interfaces of this one
  private def getSuperclasses(c: Class[_] = infoClass, stack:List[String] = List.empty[String]): List[String] = 
    val ammendedStack = (stack :+ c.getName) ::: c.getInterfaces.toList.map(_.getName)
    val sc = c.getSuperclass()
    if( sc == classOf[Object] || sc == null)
      ammendedStack
    else 
      getSuperclasses(sc, ammendedStack)

  lazy val mixins = getSuperclasses()
 

  // Fields may be self-referencing, so we need to unwind this...
  lazy val fields = _fields.map( f => f.fieldType match {
    case s: SelfRefRType => f.asInstanceOf[JavaFieldInfo].copy(fieldType = s.resolve)
    case s => f
  })

  val typeMembers: Array[TypeMemberInfo] = Nil.toArray  // unused for Java classes but needed on ClassInfo

  def show(tab:Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName + s"($name)\n"
  */