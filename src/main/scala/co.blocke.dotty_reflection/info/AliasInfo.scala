package co.blocke.dotty_reflection
package info

case class AliasInfo protected[dotty_reflection] (
    definedType: String,
    unwrappedType: RType // Aliases with a parameterized wrapped type are not currently supported, so ConcreteType here.
  ) extends RType:

    val name: String = definedType.drop(definedType.lastIndexOf('.')+1)

    val infoClass = unwrappedType.infoClass

    val orderedTypeParameters = List.empty[TypeSymbol] // unused for aliases

    def show(tab: Int = 0, supressIndent: Boolean = false, modified: Boolean = false): String = 
      val newTab = {if supressIndent then tab else tab+1}
      {if(!supressIndent) tabs(tab) else ""} + s"alias $name defined as " + unwrappedType.show(newTab,true)

