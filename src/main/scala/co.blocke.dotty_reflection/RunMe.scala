package co.blocke.dotty_reflection
import impl._

import scala.util.Try

// WORKS!
// case class Foom[X](x:List[X])
// case class Level2[Z](z: Z)
// class Foom2[Y](val y:List[Y])
// trait SomeBase[T,U]{ val t: Foom[Level2[T]]; val u: Foom2[U] }
// case class SomeThing[A,B](t: Foom[Level2[A]], u: Foom2[B]) extends SomeBase[A,B]


// case class Foom[X](x:List[X])
// case class Level2[Z](z: Z)
// class Foom2[Y](val y:List[Y])
// trait SomeBase[T,U]{ val t: Foom[T]; val u: Foom2[U] }
// case class SomeThing[A,B](t: Foom[A], u: Foom2[B]) extends SomeBase[A,B]



object RunMe extends App:


  // WORKS
  // val result = RType.inTermsOf[SomeBase[Int,Short]](Class.forName("co.blocke.dotty_reflection.SomeThing"))

  // BROKEN
  // val result = RType.inTermsOf[SomeBase[Level2[Int],Short]](Class.forName("co.blocke.dotty_reflection.SomeThing"))
  // println(result.show())

  // NOTES:
  // PRE for broken result is correct!
  // ITO is correct!
  // Result is NOT correct...

  println("done.")
