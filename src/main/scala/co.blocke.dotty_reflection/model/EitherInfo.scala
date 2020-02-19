package co.blocke.dotty_reflection
package model

case class ScalaEitherInfo(
  name: String,
  leftParamType: ALL_TYPE,
  rightParamType: ALL_TYPE
) extends ReflectedThing {
  val typeParameters = Nil
  def isA(c: Class[_]): Boolean = false // TODO
}