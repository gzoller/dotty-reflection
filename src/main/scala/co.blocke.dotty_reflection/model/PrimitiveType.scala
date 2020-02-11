package co.blocke.dotty_reflection
package model


enum PrimitiveType(baseClass: String) extends IsAable 
  def isA(c: Class[_]): Boolean = c.getName == baseClass

  case Scala_Boolean extends PrimitiveType("java.lang.Boolean")
  case Scala_Byte extends PrimitiveType("java.lang.Byte")
  case Scala_Char extends PrimitiveType("java.lang.Character")
  case Scala_Double extends PrimitiveType("java.lang.Double")
  case Scala_Float extends PrimitiveType("java.lang.Float")
  case Scala_Int extends PrimitiveType("java.lang.Integer")
  case Scala_Long extends PrimitiveType("java.lang.Long")
  case Scala_Short extends PrimitiveType("java.lang.Short")
  case Scala_String extends PrimitiveType("java.lang.String")
