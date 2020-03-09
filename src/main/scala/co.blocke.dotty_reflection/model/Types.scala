package co.blocke.dotty_reflection
package model

opaque type TypeSymbol = String // Placeholder type, e.g. T as in Foo[T](x: T)

trait ConcreteType:
  val name: String
  val typeParameters: List[TypeSymbol]

/** This is for all the classes we don't inspect.  These may be "invalid" or just not reflectable.
  * Rather than toss our exception cookies, we just return UnknownInfo and let the caller decide
  * how serious this is.  In the case of ScalaJack, it may be completely fine, for example UUID.
  * We can make a ScalaJack TypeAdapter for UUID without needing to inspect the type.  For some
  * other application an UnknownInfo might be a serious problem.
  */
case class UnknownInfo(clazz: Class[_]) extends ConcreteType:
  val name = clazz.getName
  val typeParameters = Nil

case class ScalaObjectInfo(clazz: Class[_]) extends ConcreteType:
  val name = clazz.getName
  val typeParameters = Nil

type ALL_TYPE = ConcreteType | TypeSymbol

enum PrimitiveType(val name: String) extends ConcreteType:
  def canAssign(arg: Object): Boolean = arg.getClass.getName == name
  val hasUnion = false
  val typeParameters: List[TypeSymbol] = Nil

  case Scala_Boolean extends PrimitiveType("java.lang.Boolean")
  case Scala_Byte extends PrimitiveType("java.lang.Byte")
  case Scala_Char extends PrimitiveType("java.lang.Character")
  case Scala_Double extends PrimitiveType("java.lang.Double")
  case Scala_Float extends PrimitiveType("java.lang.Float")
  case Scala_Int extends PrimitiveType("java.lang.Integer")
  case Scala_Long extends PrimitiveType("java.lang.Long")
  case Scala_Short extends PrimitiveType("java.lang.Short")
  case Scala_String extends PrimitiveType("java.lang.String")
  case Java_Object extends PrimitiveType("java.lang.Object")
  case Scala_Any extends PrimitiveType("scala.Any")
