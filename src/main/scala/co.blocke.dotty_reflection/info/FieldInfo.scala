package co.blocke.dotty_reflection
package info

import java.lang.reflect.Method

trait FieldInfo:
  val index:                Int
  val name:                 String
  val fieldType:            RType
  val annotations:          Map[String,Map[String,String]]
  val valueAccessor:        Method
  val defaultValueAccessor: Option[()=>Object]

  def show(tab: Int = 0, supressIndent: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} 
      + s"($index) $name: " 
      + fieldType.show(newTab,true) 
      + { if annotations.nonEmpty then tabs(newTab) + "annotations: " + annotations.toString + "\n" else "" }


case class ScalaFieldInfo(
  index:                Int,
  name:                 String,
  fieldType:            RType,
  annotations:          Map[String,Map[String,String]],
  valueAccessor:        Method,
  defaultValueAccessor: Option[()=>Object]
) extends FieldInfo:
  def valueOf(target: Object) = valueAccessor.invoke(target)
  def constructorClass: Class[_] = constructorClassFor(fieldType)

  private def constructorClassFor(t: RType): Class[_] = 
    t match {
      case _: TypeSymbolInfo => classOf[Object] // Magic: Java constructors set param type to Object if it is a parameterized type
      // case info: UnionInfo => classOf[Object]  // Union-typed constructors translate to Object in Java, so...
      // case info: AliasInfo => info.unwrappedType.infoClass
      case info: ScalaClassInfo if info.isValueClass => info.fields.head.fieldType.infoClass
      // case info: ScalaEnumerationInfo => Class.forName("scala.Enumeration$Value")
      case info => info.infoClass
    }

/* This is also used for Scala plain-class getter/setter fields */
case class JavaFieldInfo(
  index:           Int,
  name:            String,
  fieldType:       RType,
  annotations:     Map[String,Map[String,String]],
  valueAccessor:   Method,
  valueSetter:     Method
) extends FieldInfo:
  val defaultValueAccessor = None
