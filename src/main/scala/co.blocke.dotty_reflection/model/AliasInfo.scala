package co.blocke.dotty_reflection
package model


case class AliasInfo protected (
  definedType: String,
  unwrappedType: ALL_TYPE
  ) extends ReflectedThing:
    val name: String = definedType.drop(definedType.lastIndexOf('.')+1)
    val isUnion = unwrappedType.isInstanceOf[AliasInfo]
    def isA(c: Class[_]): Boolean = unwrappedType match {
      case i: IsAable => i.isA(c)
      case _ => false
    }
    val typeParameters = List.empty[TypeSymbol] // unused for aliases
