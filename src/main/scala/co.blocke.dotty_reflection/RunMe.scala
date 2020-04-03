package co.blocke.dotty_reflection


case class Thing(t:Array[Array[Int]])

object RunMe extends App:


  println(Reflector.reflectOn[Thing])

  /*
  ScalaClassInfo(
    co.blocke.dotty_reflection.Thing,
    class co.blocke.dotty_reflection.Thing,
    List(),
    List(
      ScalaFieldInfo(0,t,
      ArrayInfo(
        [[I,
        class scala.Array,
        ArrayInfo([I,
        class scala.Array,
        Scala_Int)),Map(),public int[][] co.blocke.dotty_reflection.Thing.t(),None)),List(),Map(),false)
  */
