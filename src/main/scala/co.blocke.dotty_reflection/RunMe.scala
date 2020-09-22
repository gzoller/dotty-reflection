package co.blocke.dotty_reflection

import java.nio.ByteBuffer

trait Pet[T] {
  val name: String
  val numLegs: Int
  val special: T
  }
case class Dog[T](name: String, numLegs: Int, special: T) extends Pet[T]

trait Foo[A]:
  val a: Option[A]

case class Bar[B]( b: B, a: Option[List[B]] ) extends Foo[List[B]]


trait Message[M]:
  val msg: M

trait CommandMessage[C] extends Message[C]

case class Command[X](msg: X) extends CommandMessage[X]


object RunMe extends App:

// tree.tpe.appliedRef

  // println(RType.of[Pet[Boolean]])
  // println(RType.of[Dog[_]])
  // println(RType.inTermsOf[Pet[Boolean]](classOf[Dog[_]]))

  // println(RType.inTermsOf[Message[String]](classOf[Command[_]]))

  // println(RType.inTermsOf[Foo[Float]](classOf[Bar[_]]))


  println(RType.of(classOf[Command[_]]))

  

  // println(RType.inTermsOf[Foo[List[Boolean]]]( classOf[Bar[_]]))
  
  // val r = RType.of[Foo[List[Boolean]]].asInstanceOf[info.TraitInfo]
  // println(r.actualParameterTypes.toList)
  // println(r.paramSymbols.toList)
  
  println("Done.")
  



// RType.inTermsOf[Foo[List[Boolean]]]( classOf[Bar] )
//   A -> List[Boolean]
