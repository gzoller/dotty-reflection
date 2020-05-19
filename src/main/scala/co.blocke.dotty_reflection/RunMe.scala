package co.blocke.dotty_reflection

import impl._ 
import info._


object Food extends Enumeration {
  val Seeds, Meat, Pellets, Veggies = Value
}

case class Foom( a: Food.Value )

enum Month {
  case Jan, Feb, Mar
}


object RunMe extends App:

  println( Reflector.reflectOn[Food.Value])
  // println(analyzeType[Food.Value])
  