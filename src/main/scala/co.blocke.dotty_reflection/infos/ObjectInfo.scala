package co.blocke.dotty_reflection
package infos

case class ObjectInfo protected[dotty_reflection](
  name: String,
  infoClass: Class[_]
  ) extends ConcreteType:
  val typeParameters: List[TypeSymbol] = Nil
