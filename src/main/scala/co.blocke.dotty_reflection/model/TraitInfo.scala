package co.blocke.dotty_reflection
package model


case class TraitInfo protected[dotty_reflection](
  name: String, 
  infoClass: Class[_], 
  typeParameters: List[TypeSymbol],
  actualParameterTypes: List[ALL_TYPE]) extends ConcreteType with ClassOrTrait:
  def setActualTypeParameters( params: List[ALL_TYPE] ) = this.copy(actualParameterTypes = params)


case class SealedTraitInfo protected(
  name: String, 
  infoClass: Class[_], 
  typeParameters: List[TypeSymbol],
  children: List[ConcreteType]) extends ConcreteType with ClassOrTrait
