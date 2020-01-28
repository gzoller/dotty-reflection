package co.blocke.dotty_reflection

import com.mypkg.Dog
import com.mypkg.sub.Person

@main def runme(): Unit =
  val found = Reflector.reflectOn[Dog[Person]]
  println("Found class "+found.name)
  println("Fields with defaults: ")
  println(found.fields.filter(_.defaultValueAccessor.isDefined).map{f => 
    f.name + " has default value of " + f.defaultValueAccessor.get()
    })
