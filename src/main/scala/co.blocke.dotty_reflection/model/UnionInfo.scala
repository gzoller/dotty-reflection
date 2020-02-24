package co.blocke.dotty_reflection
package model


case class StaticUnionInfo protected (
  val name: String,
  val typeParameters: List[TypeSymbol],
  val unionTypes: List[ALL_TYPE]
  ) extends ConcreteType
