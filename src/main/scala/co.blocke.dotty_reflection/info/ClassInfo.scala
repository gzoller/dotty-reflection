package co.blocke.dotty_reflection
package info

import impl.ClassOrTrait

trait ClassInfo extends RType with ClassOrTrait:
  val name:                  String
  val fields:                List[FieldInfo]
  val orderedTypeParameters: List[TypeSymbol]
  val annotations:           Map[String, Map[String,String]]
  def constructWith[T](args: List[Object]): T 


case class ScalaClassInfo protected[dotty_reflection] (
    name:                  String,
    infoClass:             Class[_],
    orderedTypeParameters: List[TypeSymbol],
    typeMembers:           List[TypeMemberInfo],
    fields:                List[FieldInfo],
    annotations:           Map[String, Map[String,String]],
    isValueClass:          Boolean
  ) extends ClassInfo:

  lazy val constructor = 
    infoClass.getConstructor(fields.map(_.asInstanceOf[ScalaFieldInfo].constructorClass):_*)

  def constructWith[T](args: List[Object]): T = constructor.newInstance(args:_*).asInstanceOf[T]

  def setActualTypeParams( actuals: List[TypeMemberInfo] ) = this.copy(typeMembers = actuals)

  // Used for ScalaJack writing of type members ("external type hints").  If some type members are not class/trait, it messes up any
  // type hint modifiers, so for the purposes of serialization we want to filter out "uninteresting" type members (e.g. primitives)
  def filterTraitTypeParams: ScalaClassInfo = this.copy( typeMembers = typeMembers.filter(tm => tm.memberType.isInstanceOf[TraitInfo] || tm.memberType.isInstanceOf[ScalaClassInfo]) )

  def show(tab:Int = 0, supressIndent: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}

    {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName 
    + {if isValueClass then "--Value Class--" else ""}
    + s"($name" + {if orderedTypeParameters.nonEmpty then s"""[${orderedTypeParameters.mkString(",")}]):\n""" else "):\n"}
    + tabs(newTab) + "fields:\n" + fields.map(_.show(newTab+1)).mkString
    + {if annotations.nonEmpty then tabs(newTab) + "annotations: "+annotations.toString + "\n" else ""}
    + {if( typeMembers.nonEmpty ) tabs(newTab) + "type members:\n" + typeMembers.map(_.show(newTab+1)).mkString else ""}

  
case class JavaClassInfo protected[dotty_reflection] (
    name:                  String,
    infoClass:             Class[_],
    orderedTypeParameters: List[TypeSymbol],
    fields:                List[FieldInfo],
    annotations:           Map[String, Map[String,String]],
  ) extends ClassInfo:

  private val fieldsByName = fields.map(f => (f.name, f.asInstanceOf[JavaFieldInfo])).toMap

  def field(name: String): Option[JavaFieldInfo] = fieldsByName.get(name)

  def constructWith[T](args: List[Object]): T = 
    val c = Class.forName(name).getConstructors.head.newInstance()
    fields.zipWithIndex.foreach((f,a) => f.asInstanceOf[JavaFieldInfo].valueSetter.invoke(c,args(a)))
    c.asInstanceOf[T]

  def show(tab:Int = 0, supressIndent: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}

    {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName 
    + s"($name" + {if orderedTypeParameters.nonEmpty then s"""[${orderedTypeParameters.mkString(",")}]):\n""" else "):\n"}
    + tabs(newTab) + "fields:\n" + fields.map(_.show(newTab+1)).mkString
    + {if annotations.nonEmpty then tabs(newTab) + "annotations: "+annotations.toString + "\n" else ""}
  