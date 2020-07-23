package co.blocke.dotty_reflection
import impl._

trait T5[X, Y] { val thing1: X; val thing2: Y }
trait T10[X, Y] { val x: X; val y: Y }
trait T11[W, Z] { val w: W; val z: Z }
case class TBlah1[A, B](w: A, z: B) extends T11[A, B]
case class TBar7[A, B](thing1: A, thing2: B) extends T5[A, B]
case class TFoo6[A, B, C, D](x: T11[C, T5[D, A]], y: B) extends T10[T11[C, T5[D, A]], B]


trait Level1[T,U] { val t: T; val u: Option[List[U]] }
trait Base[A,B] { val a: A; val b: B }
case class L1Class[X,Y]( t: X, u: Option[List[Y]] ) extends Level1[X,Y]
case class BaseClass[X, Y, Z]( a: Level1[X,Z], b: Y ) extends Base[Level1[X,Z],Y]

trait Wow[X]{ val a: Option[X]; val b: Int }
trait Tubular[Z]{ val a: Z }
case class Wowow[Y]( u: Y, a: Option[Y], b: Int ) extends Wow[Y]

object RunMe extends App:

  // val inst: T10[T11[Int, T5[Double, Char]], String] = TFoo6(TBlah1(5, TBar7(1.2, 'Z')), "wow")
  // val result = RType.inTermsOf[T10[T11[Int, T5[Double, Char]], String]]( inst.getClass )

  // val inst: Base[Level1[String,Boolean],Long] = BaseClass(L1Class("foo",true), 3L)
  // val result = RType.inTermsOf[Base[Level1[String,Boolean],Long]](inst.getClass)

//   val inst: Base[Level1[String,Boolean],Int] = BaseClass(L1Class("foo",Some(List(true))), 3)
//   val result = RType.inTermsOf[Base[Level1[String,Boolean],Int]](inst.getClass)
//   println(result.show())

  val result = RType.inTermsOf[Wow[Boolean]](Class.forName("co.blocke.dotty_reflection.Wowow"))
  println(result.show())


// seems to work
   // val rt = RType.of[Base[Level1[String,Boolean],Int]]
   // val p = Path( List(
   //    TraitPathElement("co.blocke.dotty_reflection.Base", "a"),
   //    TraitPathElement("co.blocke.dotty_reflection.Level1", "u"),
   //    OptionPathElement(),
   //    SeqPathElement()
   // ))
   // println(p.nav(rt))

   println("done.")

  /*
  
========> Reflected Trait (Actual types)

TraitInfo(co.blocke.dotty_reflection.Base) actualParamTypes: [
   A: TraitInfo(co.blocke.dotty_reflection.Level1) actualParamTypes: [
         T: java.lang.String
         U: scala.Boolean
      ]
   B: scala.Int
]

========> Pre-Reflect Class (type symbols)

1) Detect params: X, Y, Z for BaseClass
Q: Can we unpack the "extends" for BaseClass to see the relationship of X,Y,Z in Base trait?

ScalaCaseClassInfo(co.blocke.dotty_reflection.BaseClass):
   fields:
      (0) a: TraitInfo(co.blocke.dotty_reflection.Level1) actualParamTypes: [
            T: X
            U: Z
         ]
      (1)[Y] b: Y


      Big problem!
      Trying to capture trait's fields/types is very complex, especially when you add indirection (e.g. List[T]) into the mix.
      Is there no other way???

  */
