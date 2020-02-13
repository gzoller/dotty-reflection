package co.blocke.dotty_reflection
package model

import java.lang.reflect.Method

trait FieldInfo
  val index: Int
  val name: String
  val fieldType: ALL_TYPE
  val annotations: Map[String,Map[String,String]]
  val valueAccessor: Method
  val defaultValueAccessor: Option[()=>Object]

case class ScalaFieldInfo(
  index: Int,
  name: String,
  fieldType: ALL_TYPE,
  annotations: Map[String,Map[String,String]],
  valueAccessor: Method,
  defaultValueAccessor: Option[()=>Object]
) extends FieldInfo {
  def valueOf(target: Object) = valueAccessor.invoke(target)

  def constructorClass: Class[_] = constructorClassFor(fieldType)

  private def constructorClassFor(t: ALL_TYPE): Class[_] = t match 
    case ci:StaticUnionInfo => classOf[Object]  // Union-typed constructors translate to Object in Java, so...
    case ot:AliasInfo => constructorClassFor(ot.unwrappedType)
    case ci:StaticClassInfo if ci.isValueClass => constructorClassFor(ci.fields(0).fieldType)
    case ci:ReflectedThing => Class.forName(ci.name)  // class or trait
    case PrimitiveType.Scala_Boolean => implicitly[reflect.ClassTag[Boolean]].runtimeClass
    case PrimitiveType.Scala_Byte => implicitly[reflect.ClassTag[Byte]].runtimeClass
    case PrimitiveType.Scala_Char => implicitly[reflect.ClassTag[Char]].runtimeClass
    case PrimitiveType.Scala_Double => implicitly[reflect.ClassTag[Double]].runtimeClass
    case PrimitiveType.Scala_Float => implicitly[reflect.ClassTag[Float]].runtimeClass
    case PrimitiveType.Scala_Int => implicitly[reflect.ClassTag[Int]].runtimeClass
    case PrimitiveType.Scala_Long => implicitly[reflect.ClassTag[Long]].runtimeClass
    case PrimitiveType.Scala_Short => implicitly[reflect.ClassTag[Short]].runtimeClass
    case PrimitiveType.Scala_String => classOf[String]
    case PrimitiveType.Java_Object => classOf[Object]
    case _:TypeSymbol => Class.forName("java.lang.Object")
}

case class JavaFieldInfo(
  index: Int,
  name: String,
  fieldType: ALL_TYPE,
  annotations: Map[String,Map[String,String]],
  valueAccessor: Method,
  valueSetter: Method,
  defaultValueAccessor: Option[()=>Object]
) extends FieldInfo