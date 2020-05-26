package co.blocke.dotty_reflection
package info

case class EitherInfo protected[dotty_reflection](
  name: String,
  infoClass: Class[_],
  _leftType: RType,
  _rightType: RType
) extends RType: 

  val orderedTypeParameters = infoClass.getTypeParameters.toList.map(_.getName.asInstanceOf[TypeSymbol])

  lazy val leftType: RType = _leftType match {
    case e: SelfRefRType => e.resolve
    case e => e
  }
  lazy val rightType: RType = _rightType match {
    case e: SelfRefRType => e.resolve
    case e => e
  }

  def show(tab: Int = 0, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + "Either:\n"
    + tabs(newTab)+ "left--" + leftType.show(newTab+1,true)
    + tabs(newTab)+ "right--" + rightType.show(newTab+1,true)
