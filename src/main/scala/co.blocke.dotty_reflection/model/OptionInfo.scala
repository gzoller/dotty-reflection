package co.blocke.dotty_reflection
package model

trait OptionInfo extends ReflectedThing with IsAable {
  val optionParamType: ALL_TYPE
}

case class ScalaOptionInfo(
  name: String,
  optionParamType: ALL_TYPE
) extends OptionInfo {
  val typeParameters = Nil
  def isA(c: Class[_]): Boolean = false // TODO
}

case class JavaOptionInfo(
  name: String,
  optionParamType: ALL_TYPE
) extends OptionInfo {
  val typeParameters = Nil
  def isA(c: Class[_]): Boolean = false // TODO
}