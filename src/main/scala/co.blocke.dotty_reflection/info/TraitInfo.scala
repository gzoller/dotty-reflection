package co.blocke.dotty_reflection
package info

import impl.ClassOrTrait

case class TraitInfo protected[dotty_reflection](
    name: String, 
    infoClass: Class[_], 
    orderedTypeParameters: List[TypeSymbol],
    actualParameterTypes: List[RType]
  ) extends ConcreteType with ClassOrTrait:

  def setActualTypeParameters( params: List[RType] ) = this.copy(actualParameterTypes = params)
  def resolveTypeParams(actualTypeMap: Map[TypeSymbol, RType]): ConcreteType = this /* TODO */

  def show(tab: Int = 0, supressIndent: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName 
    + s"($name)" + {if orderedTypeParameters.nonEmpty then s"""[${orderedTypeParameters.mkString(",")}]""" else ""}
    + {if actualParameterTypes == Nil then "\n" else ":\n"+ tabs(newTab) + "actualParamTypes:\n" + actualParameterTypes.map(_.show(newTab+1)).mkString+"\n"}


case class SealedTraitInfo protected(
  name: String, 
  infoClass: Class[_], 
  orderedTypeParameters: List[TypeSymbol],
  children: List[RType]) extends ConcreteType with ClassOrTrait:

  def resolveTypeParams(actualTypeMap: Map[TypeSymbol, RType]): ConcreteType = this

  def show(tab: Int = 0, supressIndent: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName 
    + s"($name)" + {if orderedTypeParameters.nonEmpty then s"""[${orderedTypeParameters.mkString(",")}]""" else ""}
    + {if children == Nil then "\n" else ":\n"+ tabs(newTab) + "children:\n" + children.map(_.show(newTab+1)).mkString}
  