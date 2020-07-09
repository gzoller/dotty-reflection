package co.blocke.dotty_reflection

trait Item[X,Y]{ val x: X; val y: Y }

case class Thing[R,T]( x: T, y: R ) extends Item[T, R]