package co.blocke.dotty_reflection
package info

import scala.tasty.Reflection
import java.nio.ByteBuffer

object IntersectionInfo:
  def fromBytes( bbuf: ByteBuffer ): IntersectionInfo = 
    IntersectionInfo(
      StringByteEngine.read(bbuf),
      RTypeByteEngine.read(bbuf),
      RTypeByteEngine.read(bbuf)
      )

case class IntersectionInfo protected[dotty_reflection](
  name: String,
  _leftType: Transporter.RType,
  _rightType: Transporter.RType
  ) extends Transporter.RType with LeftRightRType:

    val fullName: String = name + "[" + _leftType.fullName + "," + _rightType.fullName + "]"

    lazy val infoClass: Class[_] = impl.Clazzes.AnyClazz

    lazy val leftType: Transporter.RType = _leftType match {
      case e: SelfRefRType => e.resolve
      case e => e
    }
    lazy val rightType: Transporter.RType = _rightType match {
      case e: SelfRefRType => e.resolve
      case e => e
    }

    override def toType(reflect: Reflection): reflect.Type = 
      import reflect.{_, given _}
      AndType(leftType.toType(reflect), rightType.toType(reflect))  

    def _copy( left: Transporter.RType, right: Transporter.RType ) = this.copy(_leftType = left, _rightType = right)
    
    def toBytes( bbuf: ByteBuffer ): Unit = 
      bbuf.put( INTERSECTION_INFO )
      StringByteEngine.write(bbuf, name)
      RTypeByteEngine.write(bbuf, _leftType)
      RTypeByteEngine.write(bbuf, _rightType)