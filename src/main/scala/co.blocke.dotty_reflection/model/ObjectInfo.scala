package co.blocke.dotty_reflection
package model

case class ObjectInfo protected[dotty_reflection](
  name: String,
  infoClass: Class[_]
  ) extends ConcreteType:
  val typeParameters: List[TypeSymbol] = Nil
