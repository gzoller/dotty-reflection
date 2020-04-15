package co.blocke.dotty_reflection
package info

import scala.util.Try

case class TryInfo protected[dotty_reflection](
  name: String,
  infoClass: Class[_],
  tryType: RType
) extends RType:
  val orderedTypeParameters = infoClass.getTypeParameters.toList.map(_.getName.asInstanceOf[TypeSymbol])

  def show(tab: Int = 0, supressIndent: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + s"Try of " + tryType.show(newTab,true)
