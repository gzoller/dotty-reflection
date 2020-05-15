package co.blocke.dotty_reflection
package info

case class TupleInfo protected[dotty_reflection](
  name: String,
  infoClass: Class[_],
  tupleTypes: List[RType]
) extends RType:

  val orderedTypeParameters = infoClass.getTypeParameters.toList.map(_.getName.asInstanceOf[TypeSymbol])

  def show(tab: Int = 0, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + s"""(\n${tupleTypes.map(_.show(newTab)).mkString}""" + tabs(tab) + ")\n"
