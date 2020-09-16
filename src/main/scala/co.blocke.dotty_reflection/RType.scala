package co.blocke.dotty_reflection

import scala.quoted._
import impl._
import info._
import scala.tasty.Reflection
import java.io._
import java.nio.ByteBuffer

/** This object wrapper around RType is because Serializable is not viewable from inside the compiler plugin
    code unless this is wrapped.  Don't know why--but there it is. */
object Transporter:

  val BUFFER_MAX = 65536 // max number of bytes for serialized RType tree

  /** A materializable type */
  trait RType extends Serializable:
    val name: String         /** typically the fully-qualified class name */
    val fullName: String     /** fully-qualified name w/type parameters (if AppliedType, else a copy of name) */
    override def hashCode: Int = fullName.hashCode
    override def equals(obj: Any) = this.hashCode == obj.hashCode
    lazy val infoClass: Class[_]  /** the JVM class of this type */

    // Take a parameterized type's normal type 'T' and map it to the declared type 'X'
    def resolveTypeParams( paramMap: Map[TypeSymbol, RType] ): RType = this

    // Find paths to given type symbols
    def findPaths(findSyms: Map[TypeSymbol,Path], referenceTrait: Option[TraitInfo] = None): (Map[TypeSymbol, Path], Map[TypeSymbol, Path]) = 
      (Map.empty[TypeSymbol,Path], findSyms) // (themThatsFound, themThatsStillLost)

    def toType(reflect: Reflection): reflect.Type = reflect.Type(infoClass)

    def toBytes( bbuf: ByteBuffer ): Unit

    def show(
      tab: Int = 0,
      seenBefore: List[String] = Nil,
      supressIndent: Boolean = false,
      modified: Boolean = false // modified is "special", ie. don't show index & sort for nonconstructor fields
      ): String  

    override def toString(): String = show()

    /** Write the object to a Base64 string. */
    def serialize: String =
      val buffer = ByteBuffer.allocate(BUFFER_MAX)
      toBytes(buffer)
      java.util.Base64.getEncoder().encodeToString(buffer.array.slice(0, buffer.position))
  
  /** Needed because just because something is an AppliedType doesn't mean it has parameters.  For examlpe a case class could be
   *  an applied type (isAppliedType=true) or not.  A collection is always applied.
   */
  trait AppliedRType:
    self: Transporter.RType =>
    def isAppliedType: Boolean = true  // can be overridden to false, e.g. Scala class that isn't parameterized


// Poked this here for now.  Used for show()
final inline def tabs(t:Int) = "   "*t


object RType:

  //------------------------
  //  <<  MACRO ENTRY >>
  //------------------------
  inline def of[T]: Transporter.RType = ${ ofImpl[T]() }

  inline def of(clazz: Class[_]): Transporter.RType = 
    cache.getOrElse(clazz.getName,
      unpackAnno(clazz).getOrElse{
        val tc = new TastyInspection(clazz)
        tc.inspect("", List(clazz.getName))
        tc.inspected
      }
    )

  inline def inTermsOf[T](clazz: Class[_]): Transporter.RType = 
    inTermsOf(clazz, of[T].asInstanceOf[TraitInfo])

  inline def inTermsOf(clazz: Class[_], ito: TraitInfo): Transporter.RType = 
    val clazzRType = of(clazz)
    val clazzSyms = clazz.getTypeParameters.toList.map(_.getName.asInstanceOf[TypeSymbol])
    val (symPaths, unfoundSyms) = clazzRType.findPaths(clazzSyms.map( sym => (sym->Path(Nil)) ).toMap, Some(ito))

    // Now nav the symPaths into RType from ito (reference trait) then sew these as arguments into the "naked" parameterized class (applied type)
    val paramMap = clazzSyms.map( _ match {
      case sym if unfoundSyms.contains(sym) => (sym -> PrimitiveType.Scala_Any)
      case sym => (sym -> symPaths(sym).nav(ito).getOrElse( throw new ReflectException(s"Failure to resolve type parameter '${sym}'")))
      }).toMap

    clazzRType.resolveTypeParams(paramMap)

  inline def unpackAnno(c: Class[_]): Option[Transporter.RType] =
    c.getAnnotations.toList.collectFirst{
      case s3r: S3Reflection => RType.deserialize(s3r.rtype)
    }

  // pre-loaded with known language primitive types
  private val cache = scala.collection.mutable.Map.empty[String,Transporter.RType] ++ PrimitiveType.loadCache
  def cacheSize = cache.size
  
  def ofImpl[T]()(implicit qctx: QuoteContext, ttype: scala.quoted.Type[T]): Expr[Transporter.RType] = 
    import qctx.tasty.{_, given _}
    Expr( unwindType(qctx.tasty)(typeOf[T]) )

    
  protected[dotty_reflection] def unwindType(reflect: Reflection)(aType: reflect.Type, resolveTypeSyms: Boolean = true): Transporter.RType =
    import reflect.{_, given _}

    val className = aType.asInstanceOf[TypeRef] match {
      case AndType(_,_) => INTERSECTION_CLASS
      case OrType(_,_)  => UNION_CLASS
      case normal       => normal.classSymbol.get.fullName
    }

    this.synchronized {
      val tName = typeName(reflect)(aType)
      cache.getOrElse(tName, { 
        if className == "scala.Any" then
          TastyReflection.reflectOnType(reflect)(aType, tName, resolveTypeSyms)
        else
          cache.put(tName, SelfRefRType(className))
          val reflectedRType = TastyReflection.reflectOnType(reflect)(aType, tName, resolveTypeSyms)
          cache.put(tName, reflectedRType)
          reflectedRType
      })
    }

  // Need a full name inclusive of type parameters and correcting for Enumeration's class name erasure.
  // This name is used for RType.equals so caching works.
  def typeName(reflect: Reflection)(aType: reflect.Type): String =
    import reflect.{_, given _}
    val name = aType.asInstanceOf[TypeRef] match {
      case sym if aType.typeSymbol.flags.is(Flags.Param) => sym.name
      case AppliedType(t,tob) => 
        typeName(reflect)(t) + tob.map( oneTob => typeName(reflect)(oneTob.asInstanceOf[TypeRef])).mkString("[",",","]")
      case AndType(left, right) => INTERSECTION_CLASS + "[" + typeName(reflect)(left.asInstanceOf[TypeRef]) + "," + typeName(reflect)(right.asInstanceOf[TypeRef]) + "]"
      case OrType(left, right) => UNION_CLASS + "[" + typeName(reflect)(left.asInstanceOf[TypeRef]) + "," + typeName(reflect)(right.asInstanceOf[TypeRef]) + "]"
      case _ => aType.classSymbol.get.fullName
    }
    name match {
      case ENUM_CLASSNAME => aType.asInstanceOf[TypeRef].qualifier.asInstanceOf[reflect.TermRef].termSymbol.moduleClass.fullName
      case tn => tn
    }

  def fromBytes( bbuf: ByteBuffer ): Transporter.RType = 
    bbuf.get() match {
      case SCALA_BOOLEAN         => PrimitiveType.Scala_Boolean
      case SCALA_DOUBLE          => PrimitiveType.Scala_Double
      case SCALA_INT             => PrimitiveType.Scala_Int
      case SCALA_LONG            => PrimitiveType.Scala_Long 
      case SCALA_STRING          => PrimitiveType.Scala_String
      case SCALA_ANY             => PrimitiveType.Scala_Any
      case SELFREF               => SelfRefRType.fromBytes(bbuf)
      case ALIAS_INFO            => AliasInfo.fromBytes(bbuf)
      case SCALA_CASE_CLASS_INFO => ScalaCaseClassInfo.fromBytes(bbuf)
      case SCALA_CLASS_INFO      => ScalaClassInfo.fromBytes(bbuf)
      case JAVA_CLASS_INFO       => JavaClassInfo.fromBytes(bbuf)
      case JAVA_CLASS_INFO_PROXY => JavaClassInfoProxy.fromBytes(bbuf)
      case SEQLIKE_INFO          => SeqLikeInfo.fromBytes(bbuf)
      case MAPLIKE_INFO          => MapLikeInfo.fromBytes(bbuf)
      case ARRAY_INFO            => ArrayInfo.fromBytes(bbuf)
      case EITHER_INFO           => EitherInfo.fromBytes(bbuf)
      case ENUM_INFO             => ScalaEnumInfo.fromBytes(bbuf)
      case ENUMERATION_INFO      => ScalaEnumerationInfo.fromBytes(bbuf)
      case JAVA_ENUM_INFO        => JavaEnumInfo.fromBytes(bbuf)
      case INTERSECTION_INFO     => IntersectionInfo.fromBytes(bbuf)
      case OBJECT_INFO           => ObjectInfo.fromBytes(bbuf)
      case OPTION_INFO           => ScalaOptionInfo.fromBytes(bbuf)
      case OPTIONAL_INFO         => JavaOptionalInfo.fromBytes(bbuf)
      case SCALA2_INFO           => Scala2Info.fromBytes(bbuf)
      case TRAIT_INFO            => TraitInfo.fromBytes(bbuf)
      case SEALED_TRAIT_INFO     => SealedTraitInfo.fromBytes(bbuf)
      case TRY_INFO              => TryInfo.fromBytes(bbuf)
      case TUPLE_INFO            => TupleInfo.fromBytes(bbuf)
      case TYPE_MEMBER_INFO      => TypeMemberInfo.fromBytes(bbuf)
      case TYPE_SYMBOL_INFO      => TypeSymbolInfo.fromBytes(bbuf)
      case UNION_INFO            => UnionInfo.fromBytes(bbuf)
      case UNKNOWN_INFO          => UnknownInfo.fromBytes(bbuf)
      case SCALA_BYTE            => PrimitiveType.Scala_Byte
      case SCALA_CHAR            => PrimitiveType.Scala_Char
      case SCALA_FLOAT           => PrimitiveType.Scala_Float
      case SCALA_SHORT           => PrimitiveType.Scala_Short
      case JAVA_SET_INFO         => JavaSetInfo.fromBytes(bbuf)
      case JAVA_LIST_INFO        => JavaListInfo.fromBytes(bbuf)
      case JAVA_ARRAY_INFO       => JavaArrayInfo.fromBytes(bbuf)
      case JAVA_QUEUE_INFO       => JavaQueueInfo.fromBytes(bbuf)
      case JAVA_STACK_INFO       => JavaStackInfo.fromBytes(bbuf)
      case JAVA_MAP_INFO         => JavaMapInfo.fromBytes(bbuf)
      case JAVA_BOOLEAN          => PrimitiveType.Java_Boolean
      case JAVA_BYTE             => PrimitiveType.Java_Byte
      case JAVA_CHAR             => PrimitiveType.Java_Char
      case JAVA_DOUBLE           => PrimitiveType.Java_Double
      case JAVA_FLOAT            => PrimitiveType.Java_Float
      case JAVA_INT              => PrimitiveType.Java_Int
      case JAVA_LONG             => PrimitiveType.Java_Long
      case JAVA_SHORT            => PrimitiveType.Java_Short
      case JAVA_OBJECT           => PrimitiveType.Java_Object
      case JAVA_NUMBER           => PrimitiveType.Java_Number
    }

  def deserialize( s: String ): Transporter.RType =
    val data = java.util.Base64.getDecoder().decode( s )
    val bbuf = ByteBuffer.wrap(data)
    fromBytes(bbuf)
