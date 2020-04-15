package co.blocke.dotty_reflection
package info

import java.lang.reflect._
import java.util.Optional

trait OptionInfo extends RType:
  val optionParamType: RType


case class ScalaOptionInfo protected[dotty_reflection](
  name: String,
  infoClass: Class[_],
  optionParamType: RType
) extends OptionInfo:

  val orderedTypeParameters = infoClass.getTypeParameters.toList.map(_.getName.asInstanceOf[TypeSymbol])

  def show(tab: Int = 0, supressIndent: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + "Option of " + optionParamType.show(newTab,true)


case class JavaOptionalInfo protected[dotty_reflection](
  name: String,
  infoClass: Class[_],
  optionParamType: RType
) extends OptionInfo:

  val orderedTypeParameters = infoClass.getTypeParameters.toList.map(_.getName.asInstanceOf[TypeSymbol])

  def show(tab: Int = 0, supressIndent: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + "Optional of " + optionParamType.show(newTab,true)
