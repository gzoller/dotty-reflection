package co.blocke.dotty_reflection

opaque type TypeSymbol = String // Placeholder type, e.g. T as in Foo[T](x: T)

trait ConcreteType:
  val name: String
  val typeParameters: List[TypeSymbol]
  def sewTypeParams(actualTypeMap: Map[TypeSymbol, ALL_TYPE]): ConcreteType = this

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

enum PrimitiveType(val name: String) extends ConcreteType:
  def canAssign(arg: Object): Boolean = arg.getClass.getName == name
  val hasUnion = false
  val typeParameters: List[TypeSymbol] = Nil

  case Scala_Boolean extends PrimitiveType("scala.Boolean")
  case Scala_Byte extends PrimitiveType("scala.Byte")
  case Scala_Char extends PrimitiveType("scala.Char")
  case Scala_Double extends PrimitiveType("scala.Double")
  case Scala_Float extends PrimitiveType("scala.Float")
  case Scala_Int extends PrimitiveType("scala.Int")
  case Scala_Long extends PrimitiveType("scala.Long")
  case Scala_Short extends PrimitiveType("scala.Short")
  case Scala_String extends PrimitiveType("java.lang.String")
  case Scala_Any extends PrimitiveType("scala.Any")

  case Java_Boolean extends PrimitiveType("java.lang.Boolean")
  case Java_Byte extends PrimitiveType("java.lang.Byte")
  case Java_Char extends PrimitiveType("java.lang.Character")
  case Java_Double extends PrimitiveType("java.lang.Double")
  case Java_Float extends PrimitiveType("java.lang.Float")
  case Java_Int extends PrimitiveType("java.lang.Integer")
  case Java_Long extends PrimitiveType("java.lang.Long")
  case Java_Short extends PrimitiveType("java.lang.Short")
  case Java_Object extends PrimitiveType("java.lang.Object")
