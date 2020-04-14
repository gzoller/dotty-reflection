package co.blocke.dotty_reflection

import impl.ScalaClassInspector
import info._ 

import scala.tasty.inspector._
import scala.reflect.ClassTag
import scala.jdk.CollectionConverters._

object Reflector:


  /** Runtime callable reflection on a type T.  
   * 
   * @returns RType
   */
  inline def reflectOn[T](implicit ct: ClassTag[T]): RType = 
    val typeStructure = analyzeType[T]
    cache.computeIfAbsent(typeStructure, unpackTypeStructure)


  /** Runtime callable reflection on a type structure (returned by analyzeType()).  
   * 
   * @returns RType
   */
  def reflectOnType(typeStructure: TypeStructure): RType =
    cache.computeIfAbsent(typeStructure, unpackTypeStructure)
    

  /** Same as reflectOn, except given a Class object instead of a type, T.
   *  NOTE: If Class is parameterized, this call can't infer the types of the parameters.  In that case, call reflectOnClassWithParams
   */
  def reflectOnClass(clazz: Class[_]): RType =
    val className = clazz.getName
    Option(cache.get(TypeStructure(className,Nil))).getOrElse{ 
      val tc = new ScalaClassInspector(clazz)
      tc.inspect("", List(className))
      tc.inspected
    }
  

  /** Construct a fully-parameterized RType if the class' type params are known */
  def reflectOnClassWithParams(clazz: Class[_], params: List[RType]): RType =
    val className = clazz.getName
    val tc = new ScalaClassInspector(clazz)

    // WARNING: This can fail if you inspect on a Scala library class or primitive: Int, Option, List, etc
    tc.inspect("", List(className))
    // Now sew known params into fields' type symbols, if any (type parameter resolution)
    tc.inspected match {
        /*
      case c: ScalaClassInfo => 
        val x: List[(TypeSymbol,RType)] = c.typeParameters.zip(params)
        val m: Map[TypeSymbol,RType] = x.toMap
        c.resolveTypeParams(m)
      case c: JavaClassInfo => 
        val x: List[(TypeSymbol,RType)] = c.typeParameters.zip(params)
        val m: Map[TypeSymbol,RType] = x.toMap
        c.resolveTypeParams(m)
      case c: TraitInfo =>
        c.setActualTypeParameters( params )
        */
      case c => 
        c.resolveTypeParams(c.concreteType.orderedTypeParameters.zip(params).toMap)
      }
    // tc.inspected


  // pre-loaded with known language primitive types
  private val cache = new java.util.concurrent.ConcurrentHashMap[TypeStructure, RType](Map(
      TypeStructure("boolean",Nil)              -> RType(PrimitiveType.Scala_Boolean),
      TypeStructure("Boolean",Nil)              -> RType(PrimitiveType.Scala_Boolean),
      TypeStructure("scala.Boolean",Nil)        -> RType(PrimitiveType.Scala_Boolean),
      TypeStructure("java.lang.Boolean",Nil)    -> RType(PrimitiveType.Java_Boolean),
      TypeStructure("byte",Nil)                 -> RType(PrimitiveType.Scala_Byte),
      TypeStructure("Byte",Nil)                 -> RType(PrimitiveType.Scala_Byte),
      TypeStructure("scala.Byte",Nil)           -> RType(PrimitiveType.Scala_Byte),
      TypeStructure("java.lang.Byte",Nil)       -> RType(PrimitiveType.Java_Byte),
      TypeStructure("char",Nil)                 -> RType(PrimitiveType.Scala_Char),
      TypeStructure("Char",Nil)                 -> RType(PrimitiveType.Scala_Char),
      TypeStructure("scala.Char",Nil)           -> RType(PrimitiveType.Scala_Char),
      TypeStructure("java.lang.Character",Nil)  -> RType(PrimitiveType.Java_Char),
      TypeStructure("double",Nil)               -> RType(PrimitiveType.Scala_Double),
      TypeStructure("Double",Nil)               -> RType(PrimitiveType.Scala_Double),
      TypeStructure("scala.Double",Nil)         -> RType(PrimitiveType.Scala_Double),
      TypeStructure("java.lang.Double",Nil)     -> RType(PrimitiveType.Java_Double),
      TypeStructure("float",Nil)                -> RType(PrimitiveType.Scala_Float),
      TypeStructure("Float",Nil)                -> RType(PrimitiveType.Scala_Float),
      TypeStructure("scala.Float",Nil)          -> RType(PrimitiveType.Scala_Float),
      TypeStructure("java.lang.Float",Nil)      -> RType(PrimitiveType.Java_Float),
      TypeStructure("int",Nil)                  -> RType(PrimitiveType.Scala_Int),
      TypeStructure("Int",Nil)                  -> RType(PrimitiveType.Scala_Int),
      TypeStructure("scala.Int",Nil)            -> RType(PrimitiveType.Scala_Int),
      TypeStructure("java.lang.Integer",Nil)    -> RType(PrimitiveType.Java_Int),
      TypeStructure("long",Nil)                 -> RType(PrimitiveType.Scala_Long),
      TypeStructure("Long",Nil)                 -> RType(PrimitiveType.Scala_Long),
      TypeStructure("scala.Long",Nil)           -> RType(PrimitiveType.Scala_Long),
      TypeStructure("java.lang.Long",Nil)       -> RType(PrimitiveType.Java_Long),
      TypeStructure("short",Nil)                -> RType(PrimitiveType.Scala_Short),
      TypeStructure("Short",Nil)                -> RType(PrimitiveType.Scala_Short),
      TypeStructure("scala.Short",Nil)          -> RType(PrimitiveType.Scala_Short),
      TypeStructure("java.lang.Short",Nil)      -> RType(PrimitiveType.Java_Short),
      TypeStructure("java.lang.String",Nil)     -> RType(PrimitiveType.Scala_String),
      TypeStructure("java.lang.Object",Nil)     -> RType(PrimitiveType.Java_Object),
      TypeStructure("java.lang.Number",Nil)     -> RType(PrimitiveType.Java_Number)
    ).asJava)

  /** A union type is resolved to AnyRef, which isn't helpful.  This is a marker class name to differentiate a union type */
  val UNION_CLASS = "__union_type__"

  /** An intersection type is resolved to AnyRef, which isn't helpful.  This is a marker class name to differentiate a union type */
  val INTERSECTION_CLASS = "__intersection_type__"

  /** Java Arrays devolve into java.util.List, which isn't quite the same thing, so we created this placeholder */
  val JAVA_ARRAY_CLASS = "__array__"

  /** Any is an abstract class in Scala, so Class.forName() won't work.  Need this marker. */
  val ANY_CLASS = "scala.Any"

  private def unpackTypeStructure(ps: TypeStructure): RType =
    ps match {
      case TypeStructure(ANY_CLASS, Nil) => 
        RType(PrimitiveType.Scala_Any)
      case TypeStructure(className, Nil) => 
        reflectOnClass(Class.forName(className))
      case TypeStructure(UNION_CLASS, subparams) =>
        val resolvedParams = subparams.map(sp => unpackTypeStructure(sp))
        RType(UnionInfo(UNION_CLASS, resolvedParams(0), resolvedParams(1)))
      case TypeStructure(INTERSECTION_CLASS, subparams) =>
        val resolvedParams = subparams.map(sp => unpackTypeStructure(sp))
        RType(IntersectionInfo(INTERSECTION_CLASS, resolvedParams(0), resolvedParams(1)))
      case TypeStructure(className, subparams) =>
        val resolvedParams = subparams.map(sp => unpackTypeStructure(sp))
        reflectOnClassWithParams(Class.forName(className), resolvedParams)
    }


class ReflectException(msg: String) extends Exception(msg)