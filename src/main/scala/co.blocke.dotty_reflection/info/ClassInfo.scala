package co.blocke.dotty_reflection
package info

import impl.ClassOrTrait

trait ClassInfo extends ConcreteType with ClassOrTrait:
  val name:                  String
  val fields:                List[FieldInfo]
  val orderedTypeParameters: List[TypeSymbol]
  val annotations:           Map[String, Map[String,String]]
  
  def constructWith[T](args: List[Object]): T 
  def show(tab:Int = 0, supressIndent: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}

    {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName 
    + s"($name)" + {if orderedTypeParameters.nonEmpty then s"""[${orderedTypeParameters.mkString(",")}]:\n""" else ":\n"}
    + tabs(newTab) + "fields:\n" + fields.map(_.show(newTab+1)).mkString
    + {if annotations.nonEmpty then tabs(newTab) + "annotations: "+annotations.toString + "\n" else ""}


case class ScalaClassInfo protected[dotty_reflection] (
    name:                  String,
    infoClass:             Class[_],
    orderedTypeParameters: List[TypeSymbol],
    typeMembers:           List[RType],
    fields:                List[FieldInfo],
    annotations:           Map[String, Map[String,String]],
    isValueClass:          Boolean
  ) extends ClassInfo:

  private lazy val constructor = infoClass.getConstructor(fields.map(_.asInstanceOf[ScalaFieldInfo].constructorClass):_*)

  def constructWith[T](args: List[Object]): T = constructor.newInstance(args:_*).asInstanceOf[T]

  override def show(tab:Int = 0, supressIndent: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    super.show(tab, supressIndent) 
    + {if( typeMembers.nonEmpty ) tabs(newTab) + "type members: \n" + typeMembers.map(_.show(newTab+1)) + "\n" else ""}
    + tabs(newTab) + "value class: "+isValueClass+"\n"


  /*
  override def sewTypeParams(actualTypeMap: Map[TypeSymbol, ALL_TYPE]): ConcreteType = 
    this match {
      case ci if ci.typeParameters != Nil =>
        val fixedFields = fields.map( _.sewTypeParams( actualTypeMap ))
        this.copy(fields = fixedFields)
      case ci => ci
    } match {
      case ci if ci.typeMembers != Nil =>
        val newTypeMembers = ci.typeMembers.map(tm => tm.copy(baseType = actualTypeMap(tm.typeSymbol)))
        ci.copy(typeMembers = newTypeMembers)
      case ci => ci
    }
    */

  
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

    /*
  override def sewTypeParams(actualTypeMap: Map[TypeSymbol, ALL_TYPE]) = 
    if typeParameters != Nil then // Only sew down if this class is parameterized, otherwise it's already fully resolved.
      val fixedFields = fields.map( _.sewTypeParams( actualTypeMap ))
      this.copy(fields = fixedFields)
    else 
      this
*/