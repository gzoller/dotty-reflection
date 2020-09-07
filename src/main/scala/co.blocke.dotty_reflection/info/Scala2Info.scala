package co.blocke.dotty_reflection
package info


/** RType for a Scala 2 class (no Tasty info)
 */
case class Scala2Info(name: String) extends Transporter.RType:
  val fullName = name
  lazy val infoClass = Class.forName(name)
  def show(tab: Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName + s"($name)\n"
