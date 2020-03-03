package co.blocke.dotty_reflection
package impl

object Clazzes {
  val MapClazz       = Class.forName("scala.collection.Map")
  val SetClazz       = Class.forName("scala.collection.Set")
  val SeqClazz       = Class.forName("scala.collection.Seq")
  val OptionClazz    = Class.forName("scala.Option")
  val EitherClazz    = Class.forName("scala.util.Either")
  val BooleanClazz   = Class.forName("scala.Boolean")
  val ByteClazz      = Class.forName("scala.Byte")
  val CharClazz      = Class.forName("scala.Char")
  val DoubleClazz    = Class.forName("scala.Double")
  val FloatClazz     = Class.forName("scala.Float")
  val IntClazz       = Class.forName("scala.Int")
  val LongClazz      = Class.forName("scala.Long")
  val ShortClazz     = Class.forName("scala.Short")
  val StringClazz    = Class.forName("java.lang.String") // shared Java/Scala

  // Java-specific -- lots of wrapped/primitive type stuff going on
  val booleanClazz    = java.lang.Boolean.TYPE
  val JBooleanClazz   = Class.forName("java.lang.Boolean")
  val byteClazz       = java.lang.Byte.TYPE
  val JByteClazz      = Class.forName("java.lang.Byte")
  val charClazz       = java.lang.Character.TYPE
  val JCharacterClazz = Class.forName("java.lang.Character")
  val doubleClazz     = java.lang.Double.TYPE
  val JDoubleClazz    = Class.forName("java.lang.Double")
  val floatClazz      = java.lang.Float.TYPE
  val JFloatClazz     = Class.forName("java.lang.Float")
  val intClazz        = java.lang.Integer.TYPE
  val JIntegerClazz   = Class.forName("java.lang.Integer")
  val longClazz       = java.lang.Long.TYPE
  val JLongClazz      = Class.forName("java.lang.Long")
  val shortClazz      = java.lang.Short.TYPE
  val JShortClazz     = Class.forName("java.lang.Short")

  val ObjectClazz     = Class.forName("java.lang.Object")
  val ParamTypeClazz  = Class.forName("java.lang.reflect.ParameterizedType")
  val OptionalClazz   = Class.forName("java.util.Optional")
  val JMapClazz       = classOf[java.util.Map[_,_]]
  val JListClazz      = classOf[java.util.List[_]]
  val JQueueClazz     = classOf[java.util.Queue[_]]
  val JSetClazz       = classOf[java.util.Set[_]]

  def (c: Class[_]).=:=(other: Class[_]): Boolean = c == other
  def (c: Class[_]).<:<(other: Class[_]): Boolean = other.isAssignableFrom(c)
}