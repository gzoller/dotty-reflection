package co.blocke.dotty_reflection

import impl.ScalaClassInspector
import info._ 

import impl._
import scala.tasty.inspector._
import scala.reflect.ClassTag
import scala.jdk.CollectionConverters._
import Clazzes._

object Reflector:


  /** Runtime callable reflection on a type T.  
   * 
   * @returns RType
   */
  inline def reflectOn[T](implicit ct: ClassTag[T]): RType = 
    val typeStructure = analyzeType[T]
    Option(cache.get(typeStructure)).getOrElse{ 
      unpackTypeStructure(typeStructure)
    }


  /** Runtime callable reflection on a type structure (returned by analyzeType()).  
   * 
   * @returns RType
   */
  def reflectOnType(typeStructure: TypeStructure): RType =
    Option(cache.get(typeStructure)).getOrElse{ 
      unpackTypeStructure(typeStructure)
    }
    

  /** Same as reflectOn, except given a Class object instead of a type, T.
   *  NOTE: If Class is parameterized, this call can't infer the types of the parameters.  In that case, call reflectOnClassWithParams
   */
  def reflectOnClass(clazz: Class[_], prebakedStructure: Option[TypeStructure] = None): RType =
    val className = clazz.getName
    // See if this is a top-level Scala 2 Enumeration... cumbersome, I know...
    val isEnumeration = scala.util.Try(clazz.getMethod("values")).toOption.map( _.getReturnType.getName == "scala.Enumeration$ValueSet").getOrElse(false)
    if isEnumeration then
      ScalaEnumerationInfo(className, clazz)
    else
      val structure = prebakedStructure.getOrElse(TypeStructure(className,Nil))
      this.synchronized {
        Option(cache.get(structure)).getOrElse{ 
          cache.put(structure, SelfRefRType(className, clazz))
          val tc = new ScalaClassInspector(clazz, Map.empty[TypeSymbol,RType])
          tc.inspect("", List(className))
          val found = tc.inspected
          cache.put(structure, found)
          found
        }
      }

  
  def reflectOnClassLite( clazz: Class[_] ): RType =
    val className = clazz.getName
    val structure = TypeStructure(className,Nil)
    this.synchronized {
      Option(cache.get(structure)).getOrElse{ 
        cache.put(structure, SelfRefRType(className, clazz))
        val tc = new ScalaClassInspectorLite(clazz)
        tc.inspect("", List(className))
        val found = tc.inspected
        cache.put(structure, found)
        found
      }
    }


  def reflectOnClassInTermsOf(clazz: Class[_], inTermsOf: RType): RType = 
    inTermsOf match {
      case traitInfo: TraitInfo =>
        ParamCache.resolveTypesFor(traitInfo, reflectOnClassLite(clazz)).map( paramList => reflectOnClassWithParams(clazz, paramList) )
          .getOrElse(throw new ReflectException(s"Can't resolve parentage relationship between ${inTermsOf.name} and ${clazz}"))
      case _ => throw new ReflectException("Currently, in-terms-of reflection works only for trait parents of a class. (inTermsOf is not TraitInfo)")
    }
  

  /** Construct a fully-parameterized RType if the class' type params are known */
  def reflectOnClassWithParams(clazz: Class[_], params: List[RType]): RType =
    Option(paramerterizedClassCache.get( (clazz,params) )).getOrElse{
      paramerterizedClassCache.put((clazz,params), SelfRefRType(clazz.getName, clazz, params))
      val className = clazz.getName
      val classParams = clazz.params.zip(params).toMap
      val tc = new ScalaClassInspector(clazz, classParams)

      // WARNING: This can fail if you inspect on a Scala library class or primitive: Int, Option, List, etc
      tc.inspect("", List(className))
      val found = tc.inspected
      paramerterizedClassCache.put((clazz,params), found)
      found
    }

  // pre-loaded with known language primitive types
  private val cache = new java.util.concurrent.ConcurrentHashMap[TypeStructure, RType](Map(
      TypeStructure("boolean",Nil)              -> PrimitiveType.Scala_Boolean,
      TypeStructure("Boolean",Nil)              -> PrimitiveType.Scala_Boolean,
      TypeStructure("scala.Boolean",Nil)        -> PrimitiveType.Scala_Boolean,
      TypeStructure("java.lang.Boolean",Nil)    -> PrimitiveType.Java_Boolean,
      TypeStructure("byte",Nil)                 -> PrimitiveType.Scala_Byte,
      TypeStructure("Byte",Nil)                 -> PrimitiveType.Scala_Byte,
      TypeStructure("scala.Byte",Nil)           -> PrimitiveType.Scala_Byte,
      TypeStructure("java.lang.Byte",Nil)       -> PrimitiveType.Java_Byte,
      TypeStructure("char",Nil)                 -> PrimitiveType.Scala_Char,
      TypeStructure("Char",Nil)                 -> PrimitiveType.Scala_Char,
      TypeStructure("scala.Char",Nil)           -> PrimitiveType.Scala_Char,
      TypeStructure("java.lang.Character",Nil)  -> PrimitiveType.Java_Char,
      TypeStructure("double",Nil)               -> PrimitiveType.Scala_Double,
      TypeStructure("Double",Nil)               -> PrimitiveType.Scala_Double,
      TypeStructure("scala.Double",Nil)         -> PrimitiveType.Scala_Double,
      TypeStructure("java.lang.Double",Nil)     -> PrimitiveType.Java_Double,
      TypeStructure("float",Nil)                -> PrimitiveType.Scala_Float,
      TypeStructure("Float",Nil)                -> PrimitiveType.Scala_Float,
      TypeStructure("scala.Float",Nil)          -> PrimitiveType.Scala_Float,
      TypeStructure("java.lang.Float",Nil)      -> PrimitiveType.Java_Float,
      TypeStructure("int",Nil)                  -> PrimitiveType.Scala_Int,
      TypeStructure("Int",Nil)                  -> PrimitiveType.Scala_Int,
      TypeStructure("scala.Int",Nil)            -> PrimitiveType.Scala_Int,
      TypeStructure("java.lang.Integer",Nil)    -> PrimitiveType.Java_Int,
      TypeStructure("long",Nil)                 -> PrimitiveType.Scala_Long,
      TypeStructure("Long",Nil)                 -> PrimitiveType.Scala_Long,
      TypeStructure("scala.Long",Nil)           -> PrimitiveType.Scala_Long,
      TypeStructure("java.lang.Long",Nil)       -> PrimitiveType.Java_Long,
      TypeStructure("short",Nil)                -> PrimitiveType.Scala_Short,
      TypeStructure("Short",Nil)                -> PrimitiveType.Scala_Short,
      TypeStructure("scala.Short",Nil)          -> PrimitiveType.Scala_Short,
      TypeStructure("java.lang.Short",Nil)      -> PrimitiveType.Java_Short,
      TypeStructure("java.lang.String",Nil)     -> PrimitiveType.Scala_String,
      TypeStructure("java.lang.Object",Nil)     -> PrimitiveType.Java_Object,
      TypeStructure("java.lang.Number",Nil)     -> PrimitiveType.Java_Number
    ).asJava)

  // parameterized class cache
  private val paramerterizedClassCache = new java.util.concurrent.ConcurrentHashMap[(Class[_],List[RType]), RType]

  private def unpackTypeStructure(ps: TypeStructure): RType =
    ps match {
      case TypeStructure(ANY_CLASS, Nil) => 
        PrimitiveType.Scala_Any
      case ts @ TypeStructure(className, Nil) => 
        reflectOnClass(Class.forName(className), Some(ts))
      case TypeStructure(UNION_CLASS, subparams) =>
        val resolvedParams = subparams.map(sp => unpackTypeStructure(sp))
        UnionInfo(UNION_CLASS, resolvedParams(0), resolvedParams(1))
      case TypeStructure(INTERSECTION_CLASS, subparams) =>
        val resolvedParams = subparams.map(sp => unpackTypeStructure(sp))
        IntersectionInfo(INTERSECTION_CLASS, resolvedParams(0), resolvedParams(1))
      case TypeStructure(className, subparams) =>
        val resolvedParams = subparams.map(sp => unpackTypeStructure(sp))
        reflectOnClassWithParams(Class.forName(className), resolvedParams)
    }
