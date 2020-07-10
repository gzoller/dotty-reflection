package co.blocke.dotty_reflection
package info

case class TupleInfo protected[dotty_reflection](
  name: String,
  _tupleTypes: Array[RType]
) extends RType:

  lazy val infoClass: Class[_] = Class.forName(name)

  // Elements may be self-referencing, so we need to unwind this...
  lazy val tupleTypes = _tupleTypes.map( _ match {
    case s: SelfRefRType => s.resolve
    case s => s
  })
  
  def show(tab: Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + s"""(\n${tupleTypes.map(_.show(newTab,name :: seenBefore)).mkString}""" + tabs(tab) + ")\n"
