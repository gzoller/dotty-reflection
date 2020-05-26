package co.blocke.dotty_reflection
package info

import impl.ClassOrTrait

trait ClassInfo extends RType with ClassOrTrait:
  val name:                  String
  lazy val fields:           List[FieldInfo]
  val orderedTypeParameters: List[TypeSymbol]
  val typeMembers:           List[TypeMemberInfo]
  val annotations:           Map[String, Map[String,String]]


case class ScalaCaseClassInfo protected[dotty_reflection] (
    name:                  String,
    infoClass:             Class[_],
    orderedTypeParameters: List[TypeSymbol],
    typeMembers:           List[TypeMemberInfo],
    _fields:               List[FieldInfo],
    annotations:           Map[String, Map[String,String]],
    isValueClass:          Boolean
  ) extends ClassInfo:

  // Fields may be self-referencing, so we need to unwind this...
  lazy val fields = _fields.map( f => f.fieldType match {
    case s: SelfRefRType => f.asInstanceOf[ScalaFieldInfo].copy(fieldType = s.resolve)
    case s => f
  })

  lazy val constructor = 
    infoClass.getConstructor(fields.map(_.asInstanceOf[ScalaFieldInfo].constructorClass):_*)

  def setActualTypeParams( actuals: List[TypeMemberInfo] ) = this.copy(typeMembers = actuals)

  // Used for ScalaJack writing of type members ("external type hints").  If some type members are not class/trait, it messes up any
  // type hint modifiers, so for the purposes of serialization we want to filter out "uninteresting" type members (e.g. primitives)
  def filterTraitTypeParams: ScalaCaseClassInfo = this.copy( typeMembers = typeMembers.filter(tm => tm.memberType.isInstanceOf[TraitInfo] || tm.memberType.isInstanceOf[ScalaCaseClassInfo]) )

  def show(tab:Int = 0, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}

    {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName 
    + {if isValueClass then "--Value Class--" else ""}
    + s"($name" + {if orderedTypeParameters.nonEmpty then s"""[${orderedTypeParameters.mkString(",")}]):\n""" else "):\n"}
    + tabs(newTab) + "fields:\n" + {if modified then fields.map(f => tabs(newTab+1) + f.name+s"<${f.fieldType.infoClass.getName}>\n").mkString else fields.map(_.show(newTab+1)).mkString}
    + {if annotations.nonEmpty then tabs(newTab) + "annotations: "+annotations.toString + "\n" else ""}
    + {if( typeMembers.nonEmpty ) tabs(newTab) + "type members:\n" + typeMembers.map(_.show(newTab+1)).mkString else ""}


//------------------------------------------------------------


case class ScalaClassInfo protected[dotty_reflection] (
    name:                  String,
    infoClass:             Class[_],
    orderedTypeParameters: List[TypeSymbol],
    typeMembers:           List[TypeMemberInfo],
    _fields:               List[FieldInfo],  // constructor fields
    nonConstructorFields:  List[FieldInfo],
    annotations:           Map[String, Map[String,String]],
    isValueClass:          Boolean
  ) extends ClassInfo:

  // Fields may be self-referencing, so we need to unwind this...
  lazy val fields = _fields.map( f => f.fieldType match {
    case s: SelfRefRType => f.asInstanceOf[ScalaFieldInfo].copy(fieldType = s.resolve)
    case s => f
  })
  
  lazy val constructor = 
    infoClass.getConstructor(fields.map(_.asInstanceOf[ScalaFieldInfo].constructorClass):_*)
    
  def setActualTypeParams( actuals: List[TypeMemberInfo] ) = this.copy(typeMembers = actuals)

  // Used for ScalaJack writing of type members ("external type hints").  If some type members are not class/trait, it messes up any
  // type hint modifiers, so for the purposes of serialization we want to filter out "uninteresting" type members (e.g. primitives)
  def filterTraitTypeParams: ScalaClassInfo = this.copy( typeMembers = typeMembers.filter(tm => tm.memberType.isInstanceOf[TraitInfo] || tm.memberType.isInstanceOf[ScalaCaseClassInfo]) )

  def show(tab:Int = 0, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    val showNCFields = {if !modified then nonConstructorFields else nonConstructorFields.sortBy(_.name) }

    {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName 
    + {if isValueClass then "--Value Class--" else ""}
    + s"($name" + {if orderedTypeParameters.nonEmpty then s"""[${orderedTypeParameters.mkString(",")}]):\n""" else "):\n"}
    + tabs(newTab) + "fields:\n" + fields.map(_.show(newTab+1)).mkString
    + tabs(newTab) + "non-constructor fields:\n" + showNCFields.map(_.show(newTab+1, supressIndent, modified)).mkString
    + {if annotations.nonEmpty then tabs(newTab) + "annotations: "+annotations.toString + "\n" else ""}
    + {if( typeMembers.nonEmpty ) tabs(newTab) + "type members:\n" + typeMembers.map(_.show(newTab+1)).mkString else ""}


//------------------------------------------------------------

  
case class JavaClassInfo protected[dotty_reflection] (
    name:                  String,
    infoClass:             Class[_],
    orderedTypeParameters: List[TypeSymbol],
    _fields:               List[FieldInfo],
    annotations:           Map[String, Map[String,String]],
  ) extends ClassInfo:

  // Fields may be self-referencing, so we need to unwind this...
  lazy val fields = _fields.map( f => f.fieldType match {
    case s: SelfRefRType => f.asInstanceOf[ScalaFieldInfo].copy(fieldType = s.resolve)
    case s => f
  })

  val typeMembers: List[TypeMemberInfo] = Nil  // unused for Java classes but needed on ClassInfo

  private val fieldsByName = fields.map(f => (f.name, f.asInstanceOf[JavaFieldInfo])).toMap

  def field(name: String): Option[JavaFieldInfo] = fieldsByName.get(name)

  def show(tab:Int = 0, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}

    {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName 
    + s"($name" + {if orderedTypeParameters.nonEmpty then s"""[${orderedTypeParameters.mkString(",")}]):\n""" else "):\n"}
    + tabs(newTab) + "fields:\n" + fields.map(_.show(newTab+1)).mkString
    + {if annotations.nonEmpty then tabs(newTab) + "annotations: "+annotations.toString + "\n" else ""}
  