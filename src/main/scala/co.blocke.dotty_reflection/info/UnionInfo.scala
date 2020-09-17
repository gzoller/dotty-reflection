package co.blocke.dotty_reflection
package info

import scala.tasty.Reflection
import java.nio.ByteBuffer


object UnionInfo:
  def fromBytes( bbuf: ByteBuffer ): UnionInfo = 
    UnionInfo(
      StringByteEngine.read(bbuf),
      RTypeByteEngine.read(bbuf),
      RTypeByteEngine.read(bbuf)
      )

case class UnionInfo protected[dotty_reflection] (
  name: String,
  _leftType: RType,
  _rightType: RType
  ) extends RType with LeftRightRType:

  val fullName: String = name + "[" + _leftType.fullName + "," + _rightType.fullName + "]"
  lazy val leftType: RType = _leftType match {
    case e: SelfRefRType => e.resolve
    case e => e
  }
  lazy val rightType: RType = _rightType match {
    case e: SelfRefRType => e.resolve
    case e => e
  }

  override def toType(reflect: Reflection): reflect.Type = 
    import reflect.{_, given _}
    OrType(leftType.toType(reflect), rightType.toType(reflect))

  
  def _copy( left: RType, right: RType ) = this.copy(_leftType = left, _rightType = right)

  lazy val infoClass: Class[_] = impl.Clazzes.AnyClazz

  def toBytes( bbuf: ByteBuffer ): Unit = 
    bbuf.put( UNION_INFO )
    StringByteEngine.write(bbuf, name)
    RTypeByteEngine.write(bbuf, _leftType)
    RTypeByteEngine.write(bbuf, _rightType)