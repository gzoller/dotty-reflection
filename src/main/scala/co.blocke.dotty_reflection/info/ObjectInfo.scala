package co.blocke.dotty_reflection
package info

import java.nio.ByteBuffer

object ObjectInfo:
  def fromBytes( bbuf: ByteBuffer ): ObjectInfo = 
    ObjectInfo(
      StringByteEngine.read(bbuf)
      )

case class ObjectInfo protected[dotty_reflection](
    name: String
  ) extends Transporter.RType:

  val fullName = name
  lazy val infoClass: Class[_] = Class.forName(name)

  def show(tab: Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName + s"($name)\n"

  def toBytes( bbuf: ByteBuffer ): Unit = 
    bbuf.put( OBJECT_INFO )
    StringByteEngine.write(bbuf, name)
