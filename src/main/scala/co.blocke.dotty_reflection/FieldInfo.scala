package co.blocke.dotty_reflection

import java.lang.reflect.Method

opaque type TypeSymbol = String // Placeholder type, e.g. T as in Foo[T](x: T)
enum PrimitiveType {
  case Scala_Boolean, Scala_Byte, Scala_Char, Scala_Double, Scala_Float, Scala_Int, Scala_Long, Scala_Short, Scala_String
}

case class FieldInfo(
  name: String,
  fieldType: ReflectedThing | PrimitiveType | TypeSymbol,
  valueAccessor: Method,
  defaultValueAccessor: Option[()=>Object]
) {
  def valueOf(target: Object) = valueAccessor.invoke(target)

  def constructorClass: Class[_] = fieldType match
    case ci:ReflectedThing => Class.forName(ci.name)
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

// List(public com.mypkg.sub.Person(java.lang.String,int))

// Note:  If we ever do a macro-based version of this, FieldInfo will need to be refactored into a trait.   defaultValueAccessor must be liftable,
// meaning we need to capture the name of the companion class and the method.  In the Liftable[FieldInfo] thingy we'll need to cook defaultValueAccessor
// like we do now in TastyClassConsumer