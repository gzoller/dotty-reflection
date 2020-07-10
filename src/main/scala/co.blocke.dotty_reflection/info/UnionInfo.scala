package co.blocke.dotty_reflection
package info


case class UnionInfo protected[dotty_reflection] (
  val name: String,
  val _leftType: RType,
  val _rightType: RType
  ) extends RType:

  lazy val leftType: RType = _leftType match {
    case e: SelfRefRType => e.resolve
    case e => e
  }
  lazy val rightType: RType = _rightType match {
    case e: SelfRefRType => e.resolve
    case e => e
  }

  lazy val infoClass: Class[_] = impl.Clazzes.AnyClazz

  def show(tab: Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + "Union:\n"
    + tabs(newTab)+ "left--" + leftType.show(newTab+1,name :: seenBefore,true)
    + tabs(newTab)+ "right--" + rightType.show(newTab+1,name :: seenBefore,true)
