package co.blocke.dotty_reflection
package model

case class ScalaEitherInfo(
  name: String,
  leftParamType: ALL_TYPE,
  rightParamType: ALL_TYPE
) extends ConcreteType: 
  val typeParameters = Nil