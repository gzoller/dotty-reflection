package co.blocke.dotty_reflection
package model


case class StaticTraitInfo protected(name: String, typeParameters: List[TypeSymbol]) extends ReflectedThing with ClassOrTrait with IsAable: 
  // Traits need to match the whole ecosystem graph of the arg and that of the type
  def isA(c: Class[_]): Boolean = superclassEcosystem.intersect(getSuperclasses(c)).nonEmpty
