package co.blocke.dotty_reflection

import Clazzes._

enum PrimitiveType(val name: String, val infoClass: Class[_]) extends RType:
  val orderedTypeParameters: List[TypeSymbol] = Nil

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

  def show(tab: Int = 0, supressIndent: Boolean = false, modified: Boolean = false): String = 
    {if(!supressIndent) tabs(tab) else ""} + this.name + "\n"
