package co.blocke.dotty_reflection

import model._

trait Stackable
trait Movable
trait Findable
trait Eatable
case class Foom( a: (Stackable & Movable & Findable) | String )
// case class Foom( a: Int & String & Boolean )


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

  case class S() extends Stackable with Movable with Findable
  case class T() extends Stackable with Eatable with Findable
  case class U() extends Stackable with Movable with Eatable

  try {
    println(Reflector.reflectOn[Foom])
  } catch {
    case x => //x.printStackTrace()
  }

  /*
 OrType(
   AndType(
     AndType(
       TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class blocke)),module dotty_reflection),Stackable),
       TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class blocke)),module dotty_reflection),Movable)
      ),
      TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class blocke)),module dotty_reflection),Findable)
    ),
    AndType(
      AndType(
        TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class blocke)),module dotty_reflection),Stackable),
        TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class blocke)),module dotty_reflection),Eatable)
      ),
      TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class blocke)),module dotty_reflection),Findable)
    )
  ) 

OrType(
  AndType(
    AndType(
      TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class blocke)),module dotty_reflection),Stackable),
      TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class blocke)),module dotty_reflection),Movable)
    ),
    TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class blocke)),module dotty_reflection),Findable)
  ),
  AndType(
    AndType(
      TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class blocke)),module dotty_reflection),Stackable),
      TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class blocke)),module dotty_reflection),Movable)
    ),
    TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class blocke)),module dotty_reflection),Eatable)
  )
)

AndType(
  AndType(
    TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class blocke)),module dotty_reflection),Stackable),
    TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class blocke)),module dotty_reflection),Movable)
  ),
  TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class blocke)),module dotty_reflection),Findable)
)
  */
    