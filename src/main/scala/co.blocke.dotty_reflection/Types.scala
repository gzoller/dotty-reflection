package co.blocke.dotty_reflection

import impl.Clazzes._

opaque type TypeSymbol = String // Placeholder type, e.g. T as in Foo[T](x: T)

trait ConcreteType:
  val name: String
  val infoClass: Class[_]
  val typeParameters: List[TypeSymbol]
  def sewTypeParams(actualTypeMap: Map[TypeSymbol, ALL_TYPE]): ConcreteType = this

// Marker trait for all Scala/Java collection *except* Arrays, which are a special case
trait CollectionType

/** This is for all the classes we don't inspect.  These may be "invalid" or just not reflectable.
  * Rather than toss our exception cookies, we just return UnknownInfo and let the caller decide
  * how serious this is.  In the case of ScalaJack, it may be completely fine, for example UUID.
  * We can make a ScalaJack TypeAdapter for UUID without needing to inspect the type.  For some
  * other application an UnknownInfo might be a serious problem.
  */
case class UnknownInfo(infoClass: Class[_]) extends ConcreteType:
  val name = infoClass.getName
  val typeParameters = Nil

case class ScalaObjectInfo(infoClass: Class[_]) extends ConcreteType:
  val name = infoClass.getName
  val typeParameters = Nil

type ALL_TYPE = ConcreteType | TypeSymbol

enum PrimitiveType(val name: String, val infoClass: Class[_]) extends ConcreteType:
  def canAssign(arg: Object): Boolean = arg.getClass.getName == name
  val hasUnion = false
  val typeParameters: List[TypeSymbol] = Nil

  case Scala_Boolean extends PrimitiveType("scala.Boolean", BooleanClazz)
  case Scala_Byte extends PrimitiveType("scala.Byte", ByteClazz)
  case Scala_Char extends PrimitiveType("scala.Char", CharClazz)
  case Scala_Double extends PrimitiveType("scala.Double", DoubleClazz)
  case Scala_Float extends PrimitiveType("scala.Float", FloatClazz)
  case Scala_Int extends PrimitiveType("scala.Int", IntClazz)
  case Scala_Long extends PrimitiveType("scala.Long", LongClazz)
  case Scala_Short extends PrimitiveType("scala.Short", ShortClazz)
  case Scala_String extends PrimitiveType("java.lang.String", StringClazz)
  case Scala_Any extends PrimitiveType("scala.Any", AnyClazz)

  case Java_Boolean extends PrimitiveType("java.lang.Boolean", JBooleanClazz)
  case Java_Byte extends PrimitiveType("java.lang.Byte", JByteClazz)
  case Java_Char extends PrimitiveType("java.lang.Character", JCharacterClazz)
  case Java_Double extends PrimitiveType("java.lang.Double", JDoubleClazz)
  case Java_Float extends PrimitiveType("java.lang.Float", JFloatClazz)
  case Java_Int extends PrimitiveType("java.lang.Integer", JIntegerClazz)
  case Java_Long extends PrimitiveType("java.lang.Long", JLongClazz)
  case Java_Short extends PrimitiveType("java.lang.Short", JShortClazz)
  case Java_Object extends PrimitiveType("java.lang.Object", ObjectClazz)
