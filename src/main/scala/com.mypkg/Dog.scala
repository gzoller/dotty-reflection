package com.mypkg

import sub.Person

trait Animal {
  val name: String
}

case class Dog[T](eatsKibbles: T, owner: Person, name: String = "Spot") extends Animal {
  import scala.collection.mutable.Map
  val x = "WOW"
}