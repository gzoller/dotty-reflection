package co.blocke.dotty_reflection
package info

case class AliasInfo protected[dotty_reflection] (
    definedType: String,
    unwrappedType: ConcreteType // Aliases with a typed wrapped type are not currently supported, so ConcreteType here.
  ) extends ConcreteType:

    val name: String = definedType.drop(definedType.lastIndexOf('.')+1)

    val infoClass = unwrappedType.infoClass

    val orderedTypeParameters = List.empty[TypeSymbol] // unused for aliases

    def resolveTypeParams(actualTypeMap: Map[TypeSymbol, RType]): ConcreteType = this /* TODO */

    def show(tab: Int = 0, supressIndent: Boolean = false): String = 
      val newTab = {if supressIndent then tab else tab+1}
      {if(!supressIndent) tabs(tab) else ""} + s"alias $name defined as " + unwrappedType.show(newTab,true)
  
    /*
    override def sewTypeParams(actualTypeMap: Map[TypeSymbol, ALL_TYPE]): ConcreteType = this.copy(unwrappedType = unwrappedType.sewTypeParams(actualTypeMap))
    */

