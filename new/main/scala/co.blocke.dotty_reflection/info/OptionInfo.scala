package co.blocke.dotty_reflection
package info

import java.lang.reflect._
import java.util.Optional

trait OptionInfo extends ConcreteType:
  val optionParamType: RType


case class ScalaOptionInfo protected[dotty_reflection](
  name: String,
  infoClass: Class[_],
  optionParamType: RType
) extends OptionInfo:

  val orderedTypeParameters = infoClass.getTypeParameters.toList.map(_.getName.asInstanceOf[TypeSymbol])

  def resolveTypeParams(actualTypeMap: Map[TypeSymbol, RType]): ConcreteType = 
    this.copy(optionParamType = optionParamType.resolveTypeParams(actualTypeMap)) /* TODO */

  def show(tab: Int = 0, supressIndent: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + "Option of " + optionParamType.show(newTab,true)

  /*
  override def sewTypeParams(actualTypeMap: Map[TypeSymbol, RType]): ConcreteType = optionParamType match {
    case ts: TypeSymbol if actualTypeMap.contains(ts) => this.copy(optionParamType = actualTypeMap(ts))
    case ts: TypeSymbol => this
    case c: ConcreteType => this.copy(optionParamType = c.sewTypeParams(actualTypeMap))
  }
  */


case class JavaOptionalInfo protected[dotty_reflection](
  name: String,
  infoClass: Class[_],
  optionParamType: RType
) extends OptionInfo:

  val orderedTypeParameters = infoClass.getTypeParameters.toList.map(_.getName.asInstanceOf[TypeSymbol])

  def resolveTypeParams(actualTypeMap: Map[TypeSymbol, RType]): ConcreteType = this

  def show(tab: Int = 0, supressIndent: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + "Optional of " + optionParamType.show(newTab,true)

  /*
  override def sewTypeParams(actualTypeMap: Map[TypeSymbol, RType]): ConcreteType = 
    optionParamType match {
      case ts: TypeSymbol if actualTypeMap.contains(ts) => this.copy(optionParamType = actualTypeMap(ts))
      case ts: TypeSymbol => this
      case c: ConcreteType => this.copy(optionParamType = c.sewTypeParams(actualTypeMap))
    }
    */
