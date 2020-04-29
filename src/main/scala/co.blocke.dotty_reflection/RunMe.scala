package co.blocke.dotty_reflection

import impl._ 
import info._


class Foom(a: Int, b: String)


object RunMe extends App:

  println(Reflector.reflectOn[Foom])


