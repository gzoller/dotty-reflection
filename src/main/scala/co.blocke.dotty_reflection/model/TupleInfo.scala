package co.blocke.dotty_reflection
package model

case class TupleInfo(
  name: String,
  infoClass: Class[_],
  tupleTypes: List[ALL_TYPE]
) extends ConcreteType:
  val typeParameters = Nil
