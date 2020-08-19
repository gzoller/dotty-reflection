package co.blocke.dotty_reflection

import scala.quoted._
import impl._
import info._
import scala.tasty.Reflection

/** A materializable type */
trait RType extends Serializable:
  val name: String         /** typically the fully-qualified class name */
  val fullName: String
  override def hashCode: Int = fullName.hashCode
  override def equals(obj: Any) = this.hashCode == obj.hashCode
  lazy val infoClass: Class[_]  /** the JVM class of this type */

  // Take a parameterized type's normal type 'T' and map it to the declared type 'X'
  def resolveTypeParams( paramMap: Map[TypeSymbol, RType] ): RType = this

  // Find paths to given type symbols
  def findPaths(findSyms: Map[TypeSymbol,Path], referenceTrait: Option[TraitInfo] = None): (Map[TypeSymbol, Path], Map[TypeSymbol, Path]) = 
    (Map.empty[TypeSymbol,Path], findSyms) // (themThatsFound, themThatsStillLost)

  def toType(reflect: Reflection): reflect.Type = reflect.Type(infoClass)

  def show(
    tab: Int = 0,
    seenBefore: List[String] = Nil,
    supressIndent: Boolean = false,
    modified: Boolean = false // modified is "special", ie. don't show index & sort for nonconstructor fields
    ): String  

  override def toString(): String = show()



/** Placeholder RType to be lazy-resolved, used for self-referencing types.  This is needed because without it, reflecting on
 *  a self-referencing type will enter an endless loop until the stack explodes.  This RType is immediately inserted into the
 *  type cache so that when the self-reference comes there's something in the cache to find.
 *  When one of these is encountered in the wild, just re-Reflect on the infoClass and you'll get the non-SelfRef (i.e. normal) RType
 */
case class SelfRefRType(name: String) extends RType:
  val fullName = name
  lazy val infoClass = Class.forName(name)
  def resolve = RType.of(infoClass)
  def show(tab: Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = s"SelfRefRType of $name" 


// Poked this here for now.  Used for show()
final inline def tabs(t:Int) = "   "*t


object RType:

  //------------------------
  //  <<  MACRO ENTRY >>
  //------------------------
  inline def of[T]: RType = ${ ofImpl[T]() }

  inline def of(clazz: Class[_]): RType = 
    val tc = new TastyInspection[Any](clazz)
    tc.inspect("", List(clazz.getName))
    tc.inspected

  inline def inTermsOf[T](clazz: Class[_]): RType = 
    val tc = new TastyInspection[T](clazz, Some(of[T].asInstanceOf[info.TraitInfo]))
    tc.inspect("", List(clazz.getName))
    tc.inspected

  inline def inTermsOf(clazz: Class[_], baseTrait: TraitInfo): RType =
    val tc = new TastyInspection(clazz, Some(baseTrait))
    tc.inspect("", List(clazz.getName))
    tc.inspected

  // pre-loaded with known language primitive types
  private val cache = scala.collection.mutable.Map.empty[String,RType]
  // private val cache = scala.collection.mutable.Map.empty[Object,RType] //new java.util.concurrent.ConcurrentHashMap[Object, RType]()
  def cacheSize = cache.size
  
  def ofImpl[T]()(implicit qctx: QuoteContext, ttype: scala.quoted.Type[T]): Expr[RType] = 
    import qctx.tasty.{_, given _}
    Expr( unwindType(qctx.tasty)(typeOf[T]) )

    
  protected[dotty_reflection] def unwindType(reflect: Reflection)(aType: reflect.Type, resolveTypeSyms: Boolean = true): RType =
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
          // println(s"Cache (${cache.size}) put [$tName] -> "+reflectedRType)
          reflectedRType
      })
    }

  // Need a full name inclusive of type parameters and correcting for Enumeration's class name erasure.
  // This name is used for RType.equals so caching works.
  def typeName(reflect: Reflection)( aType: reflect.Type): String =
    import reflect.{_, given _}
    val name = aType.asInstanceOf[TypeRef] match {
      case sym if aType.typeSymbol.flags.is(Flags.Param) => sym.name
      case AppliedType(t,tob) => typeName(reflect)(t) + tob.map( oneTob => typeName(reflect)(oneTob.asInstanceOf[TypeRef])).mkString("[",",","]")
      case AndType(left, right) => INTERSECTION_CLASS + "[" + typeName(reflect)(left.asInstanceOf[TypeRef]) + "," + typeName(reflect)(right.asInstanceOf[TypeRef]) + "]"
      case OrType(left, right) => UNION_CLASS + "[" + typeName(reflect)(left.asInstanceOf[TypeRef]) + "," + typeName(reflect)(right.asInstanceOf[TypeRef]) + "]"
      case _ => aType.classSymbol.get.fullName
    }
    name match {
      case ENUM_CLASSNAME => aType.asInstanceOf[TypeRef].qualifier.asInstanceOf[reflect.TermRef].termSymbol.moduleClass.fullName
      case tn => tn
    }
