package com.mypkg

import co.blocke.reflect._

import sub.Person

trait Animal {
  val name: String
}

trait SJCapture {
  var captured: java.util.HashMap[String, _] =
    new java.util.HashMap[String, Any]()
}

@ClassAnno(name="Wow")
case class Dog[T](eatsKibbles: T, @FieldAnno(idx=5) owner: Person, name: String = "Spot") extends Animal with SJCapture {
  import scala.collection.mutable.Map
  val x = "WOW"
}

opaque type PhoneNumber = String

opaque type Id = String | Int

case class Maybe( id: Id )
// In this case, we need to capture the actual arg type of T and U, and cache these, so that subsequent 
// assignments can be vetted against the actual types.