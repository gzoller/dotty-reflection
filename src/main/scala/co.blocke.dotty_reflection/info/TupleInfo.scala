package co.blocke.dotty_reflection
package info

case class TupleInfo protected[dotty_reflection](
  name: String,
  infoClass: Class[_],
  _tupleTypes: List[RType]
) extends RType:

  val orderedTypeParameters = infoClass.getTypeParameters.toList.map(_.getName.asInstanceOf[TypeSymbol])

  // Elements may be self-referencing, so we need to unwind this...
  lazy val tupleTypes = _tupleTypes.map( _ match {
    case s: SelfRefRType => s.resolve
    case s => s
  })
  
  def show(tab: Int = 0, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + s"""(\n${tupleTypes.map(_.show(newTab)).mkString}""" + tabs(tab) + ")\n"
