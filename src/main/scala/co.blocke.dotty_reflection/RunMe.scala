package co.blocke.dotty_reflection


case class Thing(t:Array[Array[Int]])

object RunMe extends App:


  println(Reflector.reflectOn[Thing])

  // val stuff = List(1,2,3)
  // val clazz = stuff.getClass
  // val name = clazz.getName

  // val cleaned = name.takeWhile(_ != '$')
  // println(cleaned)

  // val dottyName = dotty.tools.dotc.core.Names.termName(name)
  // val decoded = dotty.tools.dotc.util.NameTransformer.decode(dottyName.asSimpleName)
  // println("Decoded: "+ decoded)
  // println(Class.forName(name))
