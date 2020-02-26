package co.blocke.dotty_reflection
package model


case class StaticIntersectionInfo protected (
  val name: String,
  val typeParameters: List[TypeSymbol],
  val leftType: ALL_TYPE,
  val rightType: ALL_TYPE
  ) extends ConcreteType
