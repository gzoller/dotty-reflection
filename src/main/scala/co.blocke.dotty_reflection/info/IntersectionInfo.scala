package co.blocke.dotty_reflection
package info


case class IntersectionInfo protected[dotty_reflection](
  val name: String,
  val _leftType: RType,
  val _rightType: RType
  ) extends RType with LeftRightRType:

    lazy val infoClass: Class[_] = impl.Clazzes.AnyClazz

    lazy val leftType: RType = _leftType match {
      case e: SelfRefRType => e.resolve
      case e => e
    }
    lazy val rightType: RType = _rightType match {
      case e: SelfRefRType => e.resolve
      case e => e
    }

    def _copy( left: RType, right: RType ) = this.copy(_leftType = left, _rightType = right)
