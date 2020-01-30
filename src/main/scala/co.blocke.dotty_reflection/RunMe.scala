package co.blocke.dotty_reflection

import com.mypkg._
import com.mypkg.sub.Person

@main def runme(): Unit =
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
