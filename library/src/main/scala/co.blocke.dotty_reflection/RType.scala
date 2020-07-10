package co.blocke.dotty_reflection

import scala.quoted._
import impl._
import scala.tasty.Reflection

/** A materializable type */
trait RType extends Serializable:
  val name: String         /** typically the fully-qualified class name */
  lazy val infoClass: Class[_]  /** the JVM class of this type */

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
case class SelfRefRType(name: String, params: Array[RType] = Array.empty[RType]) extends RType:
  lazy val infoClass = Class.forName(name)
  lazy val orderedTypeParameters: List[TypeSymbol] = Nil
  def resolve = null // TODO
    // if params.isEmpty then
    //   Reflector.reflectOnClass(infoClass)
    // else
    //   Reflector.reflectOnClassWithParams(infoClass, params)
  def show(tab: Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = s"SelfRefRType of $name" 


// Poked this here for now.  Used for show()
final inline def tabs(t:Int) = "   "*t


object RType:

  //------------------------
  //  <<  MACRO ENTRY >>
  //------------------------
  inline def of[T]: RType = ${ ofImpl[T]() }

  inline def inTermsOf[T](className: String): RType = 
    val tc = new TastyInspection[T](Class.forName(className), of[T].asInstanceOf[info.TraitInfo])
    tc.inspect("", List(className))
    tc.inspected

  // pre-loaded with known language primitive types
  private val cache = new java.util.concurrent.ConcurrentHashMap[Object, RType]()
  
  def ofImpl[T]()(implicit qctx: QuoteContext, ttype: scala.quoted.Type[T]): Expr[RType] = 
    import qctx.tasty.{_, given _}
    Expr( unwindType(qctx.tasty)(typeOf[T]) )

    
  protected[dotty_reflection] def unwindType(reflect: Reflection)(aType: reflect.Type): RType =
    import reflect.{_, given _}

    val className = aType.asInstanceOf[TypeRef] match {
      case AndType(_,_) => INTERSECTION_CLASS
      case OrType(_,_)  => UNION_CLASS
      case normal       => normal.classSymbol.get.fullName
    }
    this.synchronized {
      Option(cache.get(aType)).getOrElse{ 
        // Any is a special case... It may just be an "Any", or something else, like a opaque type alias.
        // In either event, we don't want to cache the result.
        if className == "scala.Any" then
          TastyReflection.reflectOnType(reflect)(aType)
        else
          cache.put(aType, SelfRefRType(className, Nil.toArray)) // TODO! paramList.toArray))
          val reflectedRtype = TastyReflection.reflectOnType(reflect)(aType)
          cache.put(aType, reflectedRtype)
          reflectedRtype
      }
    }
