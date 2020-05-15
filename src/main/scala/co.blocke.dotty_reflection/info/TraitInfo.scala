package co.blocke.dotty_reflection
package info

import impl.ClassOrTrait

case class TraitInfo protected[dotty_reflection](
    name: String, 
    infoClass: Class[_], 
    orderedTypeParameters: List[TypeSymbol],
    actualParameterTypes: List[RType]
  ) extends RType with ClassOrTrait:

  def setActualTypeParameters( params: List[RType] ) = this.copy(actualParameterTypes = params)
  lazy val typedParams = orderedTypeParameters.zip(actualParameterTypes).toMap

  def show(tab: Int = 0, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    val both = orderedTypeParameters.zip(actualParameterTypes)

    {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName 
    + s"($name" + {if orderedTypeParameters.nonEmpty then s"""[${orderedTypeParameters.mkString(",")}])""" else ")"}
    + {if actualParameterTypes == Nil then "\n" else ":\n"+ tabs(newTab) + "actualParamTypes:\n" 
    + both.map( (p,a) => tabs(newTab+1) + s"[$p] "+a.show(newTab+2,true)).mkString}
    // + actualParameterTypes.map(_.show(newTab+1)).mkString+"\n"}


case class SealedTraitInfo protected(
    name: String, 
    infoClass: Class[_], 
    orderedTypeParameters: List[TypeSymbol],
    children: List[RType]
  ) extends RType with ClassOrTrait:

  def show(tab: Int = 0, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName 
    + s"($name" + {if orderedTypeParameters.nonEmpty then s"""[${orderedTypeParameters.mkString(",")}])""" else ")"}
    + {if children == Nil then "\n" else ":\n"+ tabs(newTab) + "children:\n" + children.map(_.show(newTab+1)).mkString}
  