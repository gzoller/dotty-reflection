package co.blocke.dotty_reflection
package model

type ALL_TYPE = ReflectedThing | PrimitiveType | TypeSymbol

opaque type TypeSymbol = String // Placeholder type, e.g. T as in Foo[T](x: T)

trait IsAable 
  def isA(c: Class[_]): Boolean

/**
 * Any reflected thing that is not a primitive type or type symbol (e.g. 'T').
 * This includes: classes of all flavors (scala, java, non-tasty scala), traits, type aliases, union types
 */
trait ReflectedThing {
  val name: String
  val typeParameters: List[TypeSymbol]
}
