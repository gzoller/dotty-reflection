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


// trait Message[M]:
//   val msg: List[M]

// trait CommandMessage[C] extends Message[C]

// case class Command[X,Y](stuff: String, one: X, msg: List[Y]) extends CommandMessage[Y]

trait Message[M,N]:
  val msg: M
  val n:   N

trait CommandMessage[C,D] extends Message[List[C],D]:
  val thing: D

case class Command[X,Y,Z](
  stuff: String, 
  one: X, 
  msg: List[Option[Y]], 
  n: Z,
  thing: Z) extends CommandMessage[Option[Y], Z]

/*
  Y and Z inTermsOf CommandMessage:
      MAPPING                DE-REF ACTION
      Y -> C                 C -> Y
      Y -> Param(0)

      Option[Z] -> D         un-apply[D] -> Z
      Option[Z] -> Param(1)  un-apply[Param(1)] -> Z

  Y and Z inTermsOf Message:  
      MAPPING                        DE-REF ACTION
      Y -> N                         N -> Y
      Y -> Param(1)                  Param(1) -> Y

      List[Option[Z]] -> M           un-apply[un-apply[M]] -> Z
      List[Option[Z]] -> Param(0)    un-apply[aun-apply[Param(0)]] -> Z
*/


object RunMe extends App:

// tree.tpe.appliedRef

  // println(RType.of[Pet[Boolean]])
  // println(RType.of[Dog[_]])
  // println(RType.inTermsOf[Pet[Boolean]](classOf[Dog[_]]))


  // println(RType.inTermsOf[Foo[Float]](classOf[Bar[_]]))


  // println(RType.of(classOf[Command[_]]))
  // println(RType.of[Command[String]])

  println(RType.inTermsOf[CommandMessage[Option[String],Boolean]](classOf[Command[_,_,_]]))
  // println(RType.inTermsOf[Message[List[String],Boolean]](classOf[Command[_,_,_]]))

  

  // println(RType.inTermsOf[Foo[List[Boolean]]]( classOf[Bar[_]]))
  
  // val r = RType.of[Foo[List[Boolean]]].asInstanceOf[info.TraitInfo]
  // println(r.actualParameterTypes.toList)
  // println(r.paramSymbols.toList)
  
  println("Done.")
  



// RType.inTermsOf[Foo[List[Boolean]]]( classOf[Bar] )
//   A -> List[Boolean]


/*
Path Map:
  Command --> CommandMessage:   C <-- Y
  Command --> Messgage:         M <-- List[Y]  UnApply[M](0) = Y  (0) is tob list index (could be > 0 for Map or Tuple)

  Goal: inTermsOf[Message[List[String]]]( classOf[Command])
*/
