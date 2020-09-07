package co.blocke.dotty_reflection
package impl

import Clazzes._

enum PrimitiveType(val name: String, val _infoClass: Class[_]) extends Transporter.RType:
  val fullName = name
  lazy val infoClass: Class[_] = _infoClass

  case Scala_Boolean extends PrimitiveType("scala.Boolean", BooleanClazz)
  case Scala_Byte    extends PrimitiveType("scala.Byte", ByteClazz)
  case Scala_Char    extends PrimitiveType("scala.Char", CharClazz)
  case Scala_Double  extends PrimitiveType("scala.Double", DoubleClazz)
  case Scala_Float   extends PrimitiveType("scala.Float", FloatClazz)
  case Scala_Int     extends PrimitiveType("scala.Int", IntClazz)
  case Scala_Long    extends PrimitiveType("scala.Long", LongClazz)
  case Scala_Short   extends PrimitiveType("scala.Short", ShortClazz)
  case Scala_String  extends PrimitiveType("java.lang.String", StringClazz)
  case Scala_Any     extends PrimitiveType("scala.Any", AnyClazz)

  case Java_Boolean  extends PrimitiveType("java.lang.Boolean", JBooleanClazz)
  case Java_Byte     extends PrimitiveType("java.lang.Byte", JByteClazz)
  case Java_Char     extends PrimitiveType("java.lang.Character", JCharacterClazz)
  case Java_Double   extends PrimitiveType("java.lang.Double", JDoubleClazz)
  case Java_Float    extends PrimitiveType("java.lang.Float", JFloatClazz)
  case Java_Int      extends PrimitiveType("java.lang.Integer", JIntegerClazz)
  case Java_Long     extends PrimitiveType("java.lang.Long", JLongClazz)
  case Java_Short    extends PrimitiveType("java.lang.Short", JShortClazz)
  case Java_Object   extends PrimitiveType("java.lang.Object", ObjectClazz)
  case Java_Number   extends PrimitiveType("java.lang.Number", JNumberClazz)

  def show(tab: Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    {if(!supressIndent) tabs(tab) else ""} + this.name + "\n"


import PrimitiveType._
object PrimitiveType:
  def unapply(symName: String): Option[Transporter.RType] = symName match {
    case "boolean"              => Some(Scala_Boolean)
    case "Boolean"              => Some(Scala_Boolean)
    case "scala.Boolean"        => Some(Scala_Boolean)
    case "java.lang.Boolean"    => Some(Java_Boolean)
    case "byte"                 => Some(Scala_Byte)
    case "Byte"                 => Some(Scala_Byte)
    case "scala.Byte"           => Some(Scala_Byte)
    case "java.lang.Byte"       => Some(Java_Byte)
    case "char"                 => Some(Scala_Char)
    case "Char"                 => Some(Scala_Char)
    case "scala.Char"           => Some(Scala_Char)
    case "java.lang.Character"  => Some(Java_Char)
    case "double"               => Some(Scala_Double)
    case "Double"               => Some(Scala_Double)
    case "scala.Double"         => Some(Scala_Double)
    case "java.lang.Double"     => Some(Java_Double)
    case "float"                => Some(Scala_Float)
    case "Float"                => Some(Scala_Float)
    case "scala.Float"          => Some(Scala_Float)
    case "java.lang.Float"      => Some(Java_Float)
    case "int"                  => Some(Scala_Int)
    case "Int"                  => Some(Scala_Int)
    case "scala.Int"            => Some(Scala_Int)
    case "java.lang.Integer"    => Some(Java_Int)
    case "long"                 => Some(Scala_Long)
    case "Long"                 => Some(Scala_Long)
    case "scala.Long"           => Some(Scala_Long)
    case "java.lang.Long"       => Some(Java_Long)
    case "short"                => Some(Scala_Short)
    case "Short"                => Some(Scala_Short)
    case "scala.Short"          => Some(Scala_Short)
    case "java.lang.Short"      => Some(Java_Short)
    case "java.lang.String"     => Some(Scala_String)
    case "java.lang.Object"     => Some(Java_Object)
    case "java.lang.Number"     => Some(Java_Number)
    case _ => None
  }
