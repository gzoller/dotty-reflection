package co.blocke.dotty_reflection
package model

import scala.util.Try

case class TryInfo(
  name: String,
  infoClass: Class[_],
  val typeParameters: List[TypeSymbol],
  tryType: ALL_TYPE
) extends ConcreteType
