package co.blocke.dotty_reflection

import model._

type Elem[X] = X match {
  case String => Char
  case Array[t] => t
  case Iterable[t] => t
}
case class Definitely( id: Elem[List[Int]], stuff: Elem[String], wow: List[Boolean] )

case class Shell(a: Char)

@main def runme(): Unit =

  /*

  // ======= Tasty Inspection-Based Reflection

  val found = Reflector.reflectOn[Dog[Person]] match {
    case ci: StaticClassInfo => ci
    case _ => null
  }
  println(found)
  val defval = found.fields(2).defaultValueAccessor.get()
  val doggo = found.constructWith[Dog[_]](List(true, Person("Greg",53), defval))
  println(doggo)
  println("Field owner: "+found.fields(1).valueOf(doggo))

  val p = Reflector.reflectOn[Project] match {
    case ci: StaticClassInfo => ci
    case _ => null
  }
  val pi = p.constructWith[Project](List(123L, p.fields(1).defaultValueAccessor.get()))
  println("Project: "+pi.id+" - "+pi.desc)
  println("Field id: "+p.fields(0).valueOf(pi))

  val t = Reflector.reflectOn[Animal]
  println(t)
  */

  // case class S() extends Stackable with Movable with Findable
  // case class T() extends Stackable with Eatable with Findable
  // case class U() extends Stackable with Movable with Eatable

  // try {
    //println(Reflector.reflectOn[Definitely])

    println(Reflector.reflectOn[Shell])

  // } catch {
  //   case x => //x.printStackTrace()
  // }
