package co.blocke.dotty_reflection

import co.blocke.reflect._

// Basic Tasty class
case class Person(name: String, age: Int, other: Int | Boolean)

// Match / dependent types
type Elem[X] = X match {
  case String => Char
  case Array[t] => t
  case Iterable[t] => t
}
case class Definitely( id: Elem[List[Int]], stuff: Elem[String] )

// Mixin tests
trait SJCapture {
  var captured: java.util.HashMap[String, _] =
    new java.util.HashMap[String, Any]()
}
class SJCaptureJava extends SJCapture

case class WithMix(id:String) extends SJCapture

// Object/field Annotations
@ClassAnno(name="Foom")
case class WithAnnotation(@FieldAnno(idx=5) id: String)

// Opaque type aliases
opaque type EMP_ID = Int
case class Employee(eId: EMP_ID, age: Int)

// Value classes
case class IdUser(id: Int) extends AnyVal  // value class
case class Employee2(eId: IdUser, age: Int)

// Parameterized classes
case class WithParam[T,U](one:T, two:U)

// Opaque type is union
opaque type GEN_ID = Int | String
case class OpaqueUnion(id: GEN_ID)

// With default value
case class WithDefault(a: Int, b: String = "wow")