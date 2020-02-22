package co.blocke.dotty_reflection
package model

opaque type TypeSymbol = String // Placeholder type, e.g. T as in Foo[T](x: T)
type ALL_TYPE = ReflectedThing | PrimitiveType | TypeSymbol
type ConcreteType = ReflectedThing | PrimitiveType  // cannot be a type symbol

trait IsAable:
  def isA(c: Class[_]): Boolean

/** Any reflected thing that is not a primitive type or type symbol (e.g. 'T').
 *  This includes: classes of all flavors (scala, java, non-tasty scala), traits, type aliases, union types
 */
trait ReflectedThing extends IsAable:
  val name: String
  val typeParameters: List[TypeSymbol]

/** Marker trait for things that can contain other things, e.g. collections.
 *  Used for determining if we need to validate down a the tree for constructWith()
 *  where Unions might be involved.  (JVM doens't prevent you from assigning an invalid-
 *  typed argument to a union type)
 */
trait UnionContainer:
  val hasUnion: Boolean
