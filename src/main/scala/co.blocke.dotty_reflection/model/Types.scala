package co.blocke.dotty_reflection
package model

opaque type TypeSymbol = String // Placeholder type, e.g. T as in Foo[T](x: T)

trait ConcreteType:
  val name: String
  val typeParameters: List[TypeSymbol]

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
