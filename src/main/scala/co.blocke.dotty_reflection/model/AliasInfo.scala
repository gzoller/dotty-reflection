package co.blocke.dotty_reflection
package model

case class AliasInfo protected (
  definedType: String,
  unwrappedType: ConcreteType
  ) extends ConcreteType:
    val name: String = definedType.drop(definedType.lastIndexOf('.')+1)
    val typeParameters = List.empty[TypeSymbol] // unused for aliases
    override def sewTypeParams(actualTypeMap: Map[TypeSymbol, ALL_TYPE]): ConcreteType = this.copy(unwrappedType = unwrappedType.sewTypeParams(actualTypeMap))

