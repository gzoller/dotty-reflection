package co.blocke.dotty_reflection

import scala.tasty.inspector._
import impl._
import scala.reflect.ClassTag
import infos._
import scala.jdk.CollectionConverters._

object Reflector:

  /** A union type is resolved to AnyRef, which isn't helpful.  This is a marker class name to differentiate a union type */
  val UNION_CLASS = "__union_type__"

  /** An intersection type is resolved to AnyRef, which isn't helpful.  This is a marker class name to differentiate a union type */
  val INTERSECTION_CLASS = "__intersection_type__"

  // NOTE: Caching used only for primitive types right now.  Let's get the rest working then
  // decide how/if we should cache other types.

  // pre-loaded with known language primitive types
  private val cache = new java.util.concurrent.ConcurrentHashMap[TypeStructure, ConcreteType](Map(
    TypeStructure("boolean",Nil)              -> PrimitiveType.Scala_Boolean,
    TypeStructure("Boolean",Nil)              -> PrimitiveType.Scala_Boolean,
    TypeStructure("scala.Boolean",Nil)        -> PrimitiveType.Scala_Boolean,
    TypeStructure("java.lang.Boolean",Nil)    -> PrimitiveType.Scala_Boolean,
    TypeStructure("byte",Nil)                 -> PrimitiveType.Scala_Byte,
    TypeStructure("Byte",Nil)                 -> PrimitiveType.Scala_Byte,
    TypeStructure("scala.Byte",Nil)           -> PrimitiveType.Scala_Byte,
    TypeStructure("java.lang.Byte",Nil)       -> PrimitiveType.Scala_Byte,
    TypeStructure("char",Nil)                 -> PrimitiveType.Scala_Char,
    TypeStructure("Char",Nil)                 -> PrimitiveType.Scala_Char,
    TypeStructure("scala.Char",Nil)           -> PrimitiveType.Scala_Char,
    TypeStructure("java.lang.Character",Nil)  -> PrimitiveType.Scala_Char,
    TypeStructure("double",Nil)               -> PrimitiveType.Scala_Double,
    TypeStructure("Double",Nil)               -> PrimitiveType.Scala_Double,
    TypeStructure("scala.Double",Nil)         -> PrimitiveType.Scala_Double,
    TypeStructure("java.lang.Double",Nil)     -> PrimitiveType.Scala_Double,
    TypeStructure("float",Nil)                -> PrimitiveType.Scala_Float,
    TypeStructure("Float",Nil)                -> PrimitiveType.Scala_Float,
    TypeStructure("scala.Float",Nil)          -> PrimitiveType.Scala_Float,
    TypeStructure("java.lang.Float",Nil)      -> PrimitiveType.Scala_Float,
    TypeStructure("int",Nil)                  -> PrimitiveType.Scala_Int,
    TypeStructure("Int",Nil)                  -> PrimitiveType.Scala_Int,
    TypeStructure("scala.Int",Nil)            -> PrimitiveType.Scala_Int,
    TypeStructure("java.lang.Integer",Nil)    -> PrimitiveType.Scala_Int,
    TypeStructure("long",Nil)                 -> PrimitiveType.Scala_Long,
    TypeStructure("Long",Nil)                 -> PrimitiveType.Scala_Long,
    TypeStructure("scala.Long",Nil)           -> PrimitiveType.Scala_Long,
    TypeStructure("java.lang.Long",Nil)       -> PrimitiveType.Scala_Long,
    TypeStructure("short",Nil)                -> PrimitiveType.Scala_Short,
    TypeStructure("Short",Nil)                -> PrimitiveType.Scala_Short,
    TypeStructure("scala.Short",Nil)          -> PrimitiveType.Scala_Short,
    TypeStructure("java.lang.Short",Nil)      -> PrimitiveType.Scala_Short,
    TypeStructure("java.lang.String",Nil)     -> PrimitiveType.Scala_String,
    TypeStructure("java.lang.Object",Nil)     -> PrimitiveType.Java_Object
  ).asJava)


  /** Runtime callable reflection on a type T.  
   * 
   * @returns ConcreteType, typically a ScalaClassInfo for a Scala class
   */
  inline def reflectOn[T](implicit ct: ClassTag[T]): ConcreteType = 
    val typeStructure = analyzeType[T]
    cache.computeIfAbsent(typeStructure, unpackTypeStructure)


  def reflectOnType(typeStructure: TypeStructure): ConcreteType =
    cache.computeIfAbsent(typeStructure, unpackTypeStructure)
    

  /** Same as reflectOn, except given a Class object instead of a type, T.
   *  NOTE: If Class is parameterized, this call can't infer the types of the parameters.
   */
  def reflectOnClass(clazz: Class[_]): ConcreteType =
    val className = clazz.getName
    Option(cache.get(TypeStructure(className,Nil))).getOrElse({ // TODO
      val tc = new ScalaClassInspector(clazz)
      tc.inspect("", List(className))
      tc.inspected
    })


  /** Reflect on an instance, including inference of type parameters.
   *  NOTE: There is a strict limitation of this reflection: You can't reflect on any collection[T] or class having a field which is a collection[T].
   *  The JVM class mechanism just throws up its hands in these situations.
   */
   /*
  def reflectOnInstance(instance: Object): ConcreteType =
    val clazz = instance.getClass
    val params = clazz.getTypeParameters.toList.map(_.getName)

    val stage1 = clazz match {
      case c if seqExtractor.matches(c) => seqExtractor.emptyInfo(c)
      case c if mapExtractor.matches(c) => mapExtractor.emptyInfo(c)
      case _ => reflectOnClass(clazz) 
    } // initial attempt at refelection

    if( params.nonEmpty ) then // if parameterized, we need to go deeper--recursively
      val paramCache = scala.collection.mutable.Map.empty[String, ConcreteType]
      stage1 match {
        case classInfo: ScalaClassInfo =>
          classInfo.copy( fields = classInfo.fields.map{ f =>
            f.fieldType match {
              case ct: ConcreteType if ct.typeParameters != Nil => replaceScalaFieldType(f, instance, paramCache)
              case ts: TypeSymbol if params.contains(ts.toString) => replaceScalaFieldType(f, instance, paramCache)
              case _ => f
              }
          })
        case _ => throw new ReflectException("This instance contains wrapped types (conllections, Option, etc.) and is not directly reflectable.  Please use a specific type when reflecting this (reflectOn[T]).")
      }
    else
      stage1

  private def replaceScalaFieldType( fieldInfo: FieldInfo, instance: Object, paramCache: scala.collection.mutable.Map[String, ConcreteType] ): FieldInfo =
    val paramTypeName = fieldInfo.fieldType.toString
    fieldInfo.asInstanceOf[ScalaFieldInfo].copy(fieldType = paramCache.get(paramTypeName).getOrElse{
      val found = reflectOnInstance( fieldInfo.valueAccessor.invoke(instance) )
      paramCache.put(paramTypeName, found)
      found
    })

  private val seqExtractor = extractors.SeqExtractor()
  private val mapExtractor = extractors.MapExtractor()
      */

  private def unpackTypeStructure(ps: TypeStructure): ConcreteType =
    ps match {
      case TypeStructure(className, Nil) => 
        reflectOnClass(Class.forName(className))
      case TypeStructure(className, subparams) if className == UNION_CLASS =>
        val resolvedParams = subparams.map(sp => unpackTypeStructure(sp))
        UnionInfo(UNION_CLASS, resolvedParams(0), resolvedParams(1))
      case TypeStructure(className, subparams) if className == INTERSECTION_CLASS =>
        val resolvedParams = subparams.map(sp => unpackTypeStructure(sp))
        IntersectionInfo(INTERSECTION_CLASS, resolvedParams(0), resolvedParams(1))
      case TypeStructure(className, subparams) =>
        val resolvedParams = subparams.map(sp => unpackTypeStructure(sp))
        reflectOnClassWithParams(Class.forName(className), resolvedParams)
    }
  

  protected[dotty_reflection] def reflectOnClassWithParams(clazz: Class[_], params: List[ALL_TYPE]): ConcreteType =
    val className = clazz.getName
    val tc = new ScalaClassInspector(clazz)
    // WARNING: This can fail if you inspect on a Scala library class or primitive: Int, Option, List, etc
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
      case c => 
        val x: List[(TypeSymbol,ALL_TYPE)] = c.typeParameters.zip(params)
        val m: Map[TypeSymbol,ALL_TYPE] = x.toMap
        c.sewTypeParams(m)
    }

class ReflectException(msg: String) extends Exception(msg)