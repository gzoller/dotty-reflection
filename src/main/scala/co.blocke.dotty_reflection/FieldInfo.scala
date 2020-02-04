package co.blocke.dotty_reflection

import java.lang.reflect.Method

opaque type TypeSymbol = String // Placeholder type, e.g. T as in Foo[T](x: T)

trait FieldInfoBase {
  val index: Int
  val name: String
  val fieldType: ALL_TYPE
  val annotations: Map[String,Map[String,String]]
}

case class FieldInfo(
  index: Int,
  name: String,
  fieldType: ALL_TYPE,
  annotations: Map[String,Map[String,String]],
  valueAccessor: Method,
  defaultValueAccessor: Option[()=>Object]
) extends FieldInfoBase {
  def valueOf(target: Object) = valueAccessor.invoke(target)

  def constructorClass: Class[_] = constructorClassFor(fieldType)

  private def constructorClassFor(t: ALL_TYPE): Class[_] = t match
    case ci:StaticUnionInfo => classOf[Object]  // Union-typed constructors translate to Object in Java, so...
    case ot:StaticAliasInfo => constructorClassFor(ot.unwrappedType)
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
    case _:TypeSymbol => Class.forName("java.lang.Object")
}

case class LiftableFieldInfo(
  index: Int,
  name: String,
  fieldType: ReflectedThing | PrimitiveType | TypeSymbol,
  annotations: Map[String,Map[String,String]]
) extends FieldInfoBase

// Note:  If we ever do a macro-based version of this, FieldInfo will need to be refactored into a trait.   defaultValueAccessor must be liftable,
// meaning we need to capture the name of the companion class and the method.  In the Liftable[FieldInfo] thingy we'll need to cook defaultValueAccessor
// like we do now in TastyClassConsumer