package co.blocke.dotty_reflection
package info


case class UnionInfo protected[dotty_reflection] (
  val name: String,
  val leftType: RType,
  val rightType: RType
  ) extends RType:

  val orderedTypeParameters: List[TypeSymbol] = Nil

  val infoClass: Class[_] = Clazzes.AnyClazz

  def show(tab: Int = 0, supressIndent: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + "Union:\n"
    + tabs(newTab)+ "left--" + leftType.show(newTab+1,true)
    + tabs(newTab)+ "right--" + rightType.show(newTab+1,true)
