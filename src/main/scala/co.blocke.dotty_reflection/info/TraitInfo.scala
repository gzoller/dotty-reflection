package co.blocke.dotty_reflection
package info


case class TraitInfo protected[dotty_reflection](
    name: String, 
    actualParameterTypes: Array[RType] = Array.empty[RType],
    paramSymbols: Array[TypeSymbol] = Array.empty[TypeSymbol]
  ) extends RType: 

  lazy val infoClass: Class[_] = Class.forName(name)

  println("??? Trait: "+name+ paramSymbols.toList.map(_.toString).mkString("[",",","]"))
  println("           "+actualParameterTypes.toList)

  def show(tab: Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}

    if seenBefore.contains(name) then
      {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName + s"($name) (self-ref recursion)\n"
    else
      val params = 
        if actualParameterTypes.isEmpty then 
          "" 
        else 
          val syms = actualParameterTypes.zip(paramSymbols)
          " actualParamTypes: [\n"+syms.map{ (ap:RType, s:TypeSymbol) => tabs(tab+1) + s.toString+": "+ap.show(tab+2,name :: seenBefore, true) }.mkString + tabs(tab) + "]"
      {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName 
      + s"($name)$params\n" 


case class SealedTraitInfo protected(
    name: String, 
    children: Array[RType]
  ) extends RType:

  lazy val infoClass: Class[_] = Class.forName(name)

  def show(tab: Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    if seenBefore.contains(name) then
      {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName + s"($name) (self-ref recursion)\n"
    else
      {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName 
      + s"($name)"
      + {if children.isEmpty then "\n" else ":\n"+ tabs(newTab) + "children:\n" + children.map(_.show(newTab+1,name :: seenBefore)).mkString}
  