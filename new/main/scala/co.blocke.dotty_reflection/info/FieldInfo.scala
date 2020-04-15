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

  def resolveTypeParams(actualTypeMap: Map[TypeSymbol, RType]): FieldInfo
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
  def resolveTypeParams(actualTypeMap: Map[TypeSymbol, RType]): FieldInfo =
    this.copy( fieldType = RType(fieldType.resolveTypeParams(actualTypeMap).concreteType, fieldType.typeParam) )

  private def constructorClassFor(t: RType): Class[_] = 
    if( t.typeParam.isDefined )
      classOf[Object] // Magic: Java constructors set param type to Object if it is a parameterized type
    else t.concreteType match { 
      case info: UnionInfo => classOf[Object]  // Union-typed constructors translate to Object in Java, so...
      case info: AliasInfo => info.unwrappedType.infoClass
      case info: ScalaClassInfo if info.isValueClass => info.fields.head.fieldType.concreteType.infoClass
      case info: ScalaEnumerationInfo => Class.forName("scala.Enumeration$Value")
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
  def resolveTypeParams(actualTypeMap: Map[TypeSymbol, RType]): FieldInfo =
    this.copy( fieldType = fieldType.resolveTypeParams(actualTypeMap) )
  /*
  def sewTypeParams(actualTypeMap: Map[TypeSymbol, ALL_TYPE]) = 
    typeParamSymbol match {
      case Some(ts) if actualTypeMap.contains(ts) =>  // 1st level direct type substitution
        this.copy(fieldType = actualTypeMap(ts) )
      case None if fieldType.isInstanceOf[ConcreteType] => // nth level -- may be a substitution--or not
        this.copy(fieldType = fieldType.asInstanceOf[ConcreteType].sewTypeParams(actualTypeMap))
      case _ => 
        this
    }
    */