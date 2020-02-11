package co.blocke.dotty_reflection
package model


case class StaticAliasInfo protected (
  definedType: String,
  unwrappedType: ALL_TYPE
  ) extends ReflectedThing with IsAable 
    val name: String = definedType.drop(definedType.lastIndexOf('.')+1)
    val isUnion = unwrappedType.isInstanceOf[StaticUnionInfo]
    def isA(c: Class[_]): Boolean = unwrappedType match {
      case i: IsAable => 
        println("Type "+definedType+ "+ isa "+c.getName+" : "+i.isA(c))
        i.isA(c)
      case _ => 
        println("Type "+definedType+ "+ isa "+c.getName+" : false")
        false
    }
    val typeParameters = List.empty[TypeSymbol] // unused for aliases
