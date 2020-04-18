package co.blocke.dotty_reflection


case class Person[Z](name: String, age: Int, other: Foo[Boolean,Z])
case class Foo[A,B](it: B, other: A)

trait G[S,T]
trait X[I] extends G[Boolean,I]
trait A[P] extends X[P]{ val x: P }
trait B[K]{ val y: K }
case class C[Q,W](x:Q, y:W) extends A[Q] with B[W]

object RunMe extends App:

  // println(Reflector.reflectOn[Person[Int]])

  // println(Reflector.reflectOn[C[Int,Boolean]])

  val t = Reflector.reflectOn[B[Boolean]].asInstanceOf[info.TraitInfo]
  println(t)

  val c = Reflector.reflectOnClass(Class.forName("co.blocke.dotty_reflection.C"))

  println(impl.ParamGraphRegistry.show)
  println("-----")
  println(impl.ParamGraphRegistry.resolveTypesFor(t,c))
  println("\n\n================================\n\n")

  println(Reflector.reflectOnClassInTermsOf(Class.forName("co.blocke.dotty_reflection.C"), t))