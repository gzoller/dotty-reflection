package co.blocke.dotty_reflection
package model

opaque type TypeSymbol = String // Placeholder type, e.g. T as in Foo[T](x: T)
type ALL_TYPE = ReflectedThing | PrimitiveType | TypeSymbol
type ConcreteType = ReflectedThing | PrimitiveType  // cannot be a type symbol

trait IsAable 
  def isA(c: Class[_]): Boolean

/**
 * Any reflected thing that is not a primitive type or type symbol (e.g. 'T').
 * This includes: classes of all flavors (scala, java, non-tasty scala), traits, type aliases, union types
 */
trait ReflectedThing extends IsAable {
  val name: String
  val typeParameters: List[TypeSymbol]
}
