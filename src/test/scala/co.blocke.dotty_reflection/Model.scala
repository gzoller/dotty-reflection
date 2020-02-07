package co.blocke.dotty_reflection

import co.blocke.reflect._

case class Person(name: String, age: Int, other: Int | Boolean)

type Elem[X] = X match {
  case String => Char
  case Array[t] => t
  case Iterable[t] => t
}
case class Definitely( id: Elem[List[Int]], stuff: Elem[String] )

trait SJCapture {
  var captured: java.util.HashMap[String, _] =
    new java.util.HashMap[String, Any]()
}
class SJCaptureJava extends SJCapture

case class WithMix(id:String) extends SJCapture

@ClassAnno(name="Foom")
case class WithAnnotation(@FieldAnno(idx=5) id: String)

opaque type EMP_ID = Int
case class Employee(eId: EMP_ID, age: Int)

case class IdUser(id: Int) extends AnyVal  // value class
case class Employee2(eId: IdUser, age: Int)

case class WithParam[T,U](one:T, two:U)