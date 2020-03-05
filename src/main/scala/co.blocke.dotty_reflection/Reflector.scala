package co.blocke.dotty_reflection

import scala.tasty.inspector._
import impl._
import scala.reflect.ClassTag
import model._
import scala.collection.mutable.HashMap

object Reflector:

  type CacheType = HashMap[String, ConcreteType]

  // pre-loaded with known language primitive types
  private val cache:CacheType = HashMap(
    "boolean"             -> PrimitiveType.Scala_Boolean,
    "Boolean"             -> PrimitiveType.Scala_Boolean,
    "java.lang.Boolean"   -> PrimitiveType.Scala_Boolean,
    "byte"                -> PrimitiveType.Scala_Byte,
    "Byte"                -> PrimitiveType.Scala_Byte,
    "java.lang.Byte"      -> PrimitiveType.Scala_Byte,
    "char"                -> PrimitiveType.Scala_Char,
    "Char"                -> PrimitiveType.Scala_Char,
    "java.lang.Character" -> PrimitiveType.Scala_Char,
    "double"              -> PrimitiveType.Scala_Double,
    "Double"              -> PrimitiveType.Scala_Double,
    "java.lang.Double"    -> PrimitiveType.Scala_Double,
    "float"               -> PrimitiveType.Scala_Float,
    "Float"               -> PrimitiveType.Scala_Float,
    "java.lang.Float"     -> PrimitiveType.Scala_Float,
    "int"                 -> PrimitiveType.Scala_Int,
    "Int"                 -> PrimitiveType.Scala_Int,
    "java.lang.Integer"   -> PrimitiveType.Scala_Int,
    "long"                -> PrimitiveType.Scala_Long,
    "Long"                -> PrimitiveType.Scala_Long,
    "java.lang.Long"      -> PrimitiveType.Scala_Long,
    "short"               -> PrimitiveType.Scala_Short,
    "Short"               -> PrimitiveType.Scala_Short,
    "java.lang.Short"     -> PrimitiveType.Scala_Short,
    "java.lang.String"    -> PrimitiveType.Scala_String,
    "java.lang.Object"    -> PrimitiveType.Java_Object
  )

  def reflectOn[T](implicit ct: ClassTag[T]): ConcreteType = 
    val clazz = ct.runtimeClass
    reflectOnClass(clazz)

  def reflectOnClass[T](clazz: Class[T]): ConcreteType =
    val className = clazz.getName
    val found: Option[ConcreteType] = cache.get(className)
    found.getOrElse({
      val tc = new ScalaClassInspector(clazz, cache)
      tc.inspect("", List(className))
      cache.get(className).getOrElse(throw new Exception(s"Unable to reflect on class $className"))
    })
