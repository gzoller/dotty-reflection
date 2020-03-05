package co.blocke.dotty_reflection
package model


case class StaticTraitInfo protected(
  name: String, 
  infoClass: Class[_], 
  typeParameters: List[TypeSymbol]) extends ConcreteType with ClassOrTrait