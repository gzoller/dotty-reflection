package co.blocke.dotty_reflection

import impl._ 
import info._


case class HooLoo(name: String, more: HooLoo)


object RunMe extends App:

  val r = Reflector.reflectOn[HooLoo].asInstanceOf[ScalaCaseClassInfo]
  println(r.fields)
  