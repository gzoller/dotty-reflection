package co.blocke.dotty_reflection
package info

import scala.tasty.Reflection

case class AliasInfo protected[dotty_reflection] (
    definedType: String,
    unwrappedType: Transporter.RType // Aliases with a parameterized wrapped type are not currently supported, so ConcreteType here.
  ) extends Transporter.RType:

    val name: String = definedType.drop(definedType.lastIndexOf('.')+1)
    val fullName = name

    lazy val infoClass = unwrappedType.infoClass

    override def toType(reflect: Reflection): reflect.Type = unwrappedType.toType(reflect)

    def show(tab: Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
      val newTab = {if supressIndent then tab else tab+1}
      {if(!supressIndent) tabs(tab) else ""} + s"alias $name defined as " + unwrappedType.show(newTab,name :: seenBefore,true)

