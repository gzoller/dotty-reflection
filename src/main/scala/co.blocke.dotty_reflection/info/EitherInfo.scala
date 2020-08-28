package co.blocke.dotty_reflection
package info

case class EitherInfo protected[dotty_reflection](
  name: String,
  _leftType: Transporter.RType,
  _rightType: Transporter.RType
) extends Transporter.RType with LeftRightRType: 

  val fullName: String = name + "[" + _leftType.fullName + "," + _rightType.fullName + "]"

  lazy val infoClass: Class[_] = Class.forName(name)

  lazy val leftType: Transporter.RType = _leftType match {
    case e: SelfRefRType => e.resolve
    case e => e
  }
  lazy val rightType: Transporter.RType = _rightType match {
    case e: SelfRefRType => e.resolve
    case e => e
  }

  def _copy( left: Transporter.RType, right: Transporter.RType ) = this.copy(_leftType = left, _rightType = right)


