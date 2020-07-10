package co.blocke.dotty_reflection

trait Item[X,Y]{ val x: X; val y: Y }

case class Thing[R,T]( x: T, y: R ) extends Item[T, R]

trait Command
case class FieldCommand(order: String) extends Command
case class Message[S, T <: Command](id:String, payload:T, s: S) {
  type payloadKind = T
}

class PlainGood[X](val a: X, val b: String)

trait InterA
trait InterB
trait InterC
case class IntersectionHolder( a: InterA & InterB & InterC )