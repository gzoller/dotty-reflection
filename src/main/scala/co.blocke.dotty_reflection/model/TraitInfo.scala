package co.blocke.dotty_reflection
package model


case class StaticTraitInfo protected(name: String, typeParameters: List[TypeSymbol]) extends ConcreteType with ClassOrTrait