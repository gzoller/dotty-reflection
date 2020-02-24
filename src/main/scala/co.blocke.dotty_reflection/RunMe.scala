package co.blocke.dotty_reflection

import model._

trait X
trait Y
trait Z
class A(a: Int) extends X
class B(a: Int) extends A(a) with Y
class C(a: Int) extends B(a) with Z

case class Foom(a: Boolean | Option[Int])


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

  // ======== Macro-based Reflection

  // println(Reflector.reflectOn[Dog[Person]].asInstanceOf[StaticClassInfo].mixins)


  // val cJava = classOf[co.blocke.reflect.FooJava]
  // println(cJava.getModule())
  // println("---------")
  // val cScala = classOf[com.mypkg.FooScala]
  // println(cScala.getModule())

  // try {
    // val f = Reflector.reflectOn[Foomx[Int]].asInstanceOf[StaticClassInfo]
    // println(f)
  // } catch {
  //   case e => println("=== BOOM! ==="+e.getStackTrace)
  // }

  // val f = Reflector.reflectOn[Definitely]
  // println(f)

  // println(f.asInstanceOf[StaticClassInfo].constructWith[Maybe](List(true)))

  // val x = 5
  // println(f)
  // println(f.asInstanceOf[StaticClassInfo].constructWith[Maybe](List(Person("Greg",53))))

  // val vc = Reflector.reflectOn[VCString].asInstanceOf[StaticClassInfo]
  // val vcObj = VCString("Foom!")
  // println(vc.fields(0).valueOf(vcObj.asInstanceOf[Object]))
  // println(vc.constructWith[VCString](List("Greg")))
    
  // def getSuperclasses(c: Class[_], stack:List[String]): List[String] = 
  //   val sc = c.getSuperclass()
  //   if( sc == classOf[Object])
  //     stack
  //   else 
  //     getSuperclasses(sc, stack :+ sc.getName)
  

  // val c = classOf[C]
  // println("Class "+c.getName)
  // println(getSuperclasses(c, List(c.getName)))
    