package com.mypkg

import co.blocke.reflect._

import sub.Person

trait Animal {
  val name: String
}

@ClassAnno(name="Wow")
case class Dog[T](eatsKibbles: T, @FieldAnno(idx=5) owner: Person, name: String = "Spot") extends Animal {
  import scala.collection.mutable.Map
  val x = "WOW"
}


case class Maybe( id: co.blocke.dotty_reflection.X | Int)