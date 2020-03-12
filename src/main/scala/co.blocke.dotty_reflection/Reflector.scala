package co.blocke.dotty_reflection

import scala.tasty.inspector._
import impl._
import scala.reflect.ClassTag
import model._
import scala.collection.mutable.HashMap

object Reflector:

  type CacheType = HashMap[String, ConcreteType]

  // NOTE: Caching used only for primitive types right now.  Let's get the rest working then
  // decide how/if we should cache other types.

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


  /** Runtime callable reflection on a type T.  Note that there is one corner case where this runtime-callable
   * function does not work:  When T is itself a parameterized type.  In that case, at runtime T's actual parameter
   * types are (un)helpfully erased by the JVM, and are un-knowable.  If you have this use case you will need to 
   * use the compile-time macro implementation of this library instead, which can handle parameterized T's, as all
   * that type information is accessable at compile-time, pre-erasure.
   * 
   * @returns ConcreteType, typically a ScalaClassInfo for a Scala class
   */
  def reflectOn[T](implicit ct: ClassTag[T]): ConcreteType = 
    val clazz = ct.runtimeClass
    reflectOnClass(clazz)


  /** Same as reflectOn, except given a Class object instead of a type, T.
   */
  def reflectOnClass(clazz: Class[_]): ConcreteType =
    val className = clazz.getName
    val found: Option[ConcreteType] = cache.get(className)
    found.getOrElse({
      val tc = new ScalaClassInspector(clazz)
      tc.inspect("", List(className))
      tc.inspected
      // cache.get(className).getOrElse(UnknownInfo(clazz))
    })


  protected[dotty_reflection] def reflectOnClassWithParams(clazz: Class[_], params: List[ALL_TYPE]): ConcreteType =
    val className = clazz.getName
    val tc = new ScalaClassInspector(clazz)
    tc.inspect("", List(className))
    // Now sew known params into fields' type symbols, if any
    tc.inspected match {
      case c: ScalaClassInfo => 
        val x: List[(TypeSymbol,ALL_TYPE)] = c.typeParameters.zip(params)
        val m: Map[TypeSymbol,ALL_TYPE] = x.toMap
        c.sewTypeParams(m)
      case c: JavaClassInfo => 
        val x: List[(TypeSymbol,ALL_TYPE)] = c.typeParameters.zip(params)
        val m: Map[TypeSymbol,ALL_TYPE] = x.toMap
        c.sewTypeParams(m)
      case c: TraitInfo =>
        c.setActualTypeParameters( params )
      case c => c
    }
