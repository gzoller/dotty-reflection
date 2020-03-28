package co.blocke.dotty_reflection

case class Person[T](name:String, age: Int, thing: List[Int], other: T)

case class Foo[X](a: X)
case class Bar[Y,Z]( c: Y, d: Z)

object RunMe extends App:
  println("Hello, world!")
  val p = Person("Greg",53,List(123,45), 5)

  val ts = impl.analyzeType[Person[Int]]
  println(ts)
  println(Reflector.reflectOnType(ts))

  // println(Reflector.reflectOn[scala.collection.immutable.HashSet[Boolean]])

  // val stuff = List(1,2,3)
  // val clazz = stuff.getClass
  // val name = clazz.getName

  // val cleaned = name.takeWhile(_ != '$')
  // println(cleaned)

  // val dottyName = dotty.tools.dotc.core.Names.termName(name)
  // val decoded = dotty.tools.dotc.util.NameTransformer.decode(dottyName.asSimpleName)
  // println("Decoded: "+ decoded)
  // println(Class.forName(name))
