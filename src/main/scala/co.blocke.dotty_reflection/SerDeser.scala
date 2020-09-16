package co.blocke.dotty_reflection

import java.io._
import java.nio._
import scala.reflect.ClassTag
import info.{FieldInfo, ScalaFieldInfo, JavaFieldInfo}

trait BytesEngine[T]:
  def write( bbuf: ByteBuffer, t: T ): Unit
  def read( bbuf: ByteBuffer ): T

/*
Things we need to serialize:

Byte -- ByteBuffer primitive
Int -- ByteBuffer primitive

// Stringish
String
TypeSymbol

Boolean

// Arrays
Array[TypeSymbol]
Array[TypeMemberInfo]
Array[FieldInfo]
Array[Transporter.RType]
Array[String]

// Maps
Map[String,Map[String,String]]
Map[TypeSymbol, Transporter.RType]

// Option
Option[TypeSymbol]
Option[(String,String)]  (can be implemented as Option[List[String]])

// Class
RType
FieldInfo
TypeMemberInfo

// Object
Method
*/


object RTypeByteEngine extends BytesEngine[Transporter.RType]:
  def write( bbuf: ByteBuffer, t: Transporter.RType ): Unit = t.toBytes(bbuf)
  def read( bbuf: ByteBuffer ): Transporter.RType = RType.fromBytes(bbuf)


object FieldInfoByteEngine extends BytesEngine[FieldInfo]:
  def write( bbuf: ByteBuffer, t: FieldInfo ): Unit = 
    t match {
      case s: ScalaFieldInfo => s.toBytes(bbuf)
      case j: JavaFieldInfo => j.toBytes(bbuf)
    }
  def read( bbuf: ByteBuffer ): FieldInfo = 
    bbuf.get() match {
      case SCALA_FIELD_INFO => ScalaFieldInfo.fromBytes(bbuf)
      case JAVA_FIELD_INFO => JavaFieldInfo.fromBytes(bbuf)
    }


object StringByteEngine extends BytesEngine[String]:
  def write( bbuf: ByteBuffer, t: String ): Unit =
    bbuf.putInt(t.length)
    bbuf.put(t.getBytes)

  def read( bbuf: ByteBuffer ): String =
    val len = bbuf.getInt()
    val byteArray = new Array[Byte](len)
    bbuf.get( byteArray, 0, len )
    new String(byteArray, 0, len)


object BooleanByteEngine extends BytesEngine[Boolean]:
  def write( bbuf: ByteBuffer, t: Boolean ): Unit =
    if t then
      bbuf.put(1.toByte)
    else
      bbuf.put(0.toByte)

  def read( bbuf: ByteBuffer ): Boolean =
    bbuf.get() match {
      case 0 => false
      case 1 => true
    }


case class ArrayByteEngine[T:ClassTag](elementEngine: BytesEngine[T]) extends BytesEngine[Array[T]]:
  def write( bbuf: ByteBuffer, t: Array[T] ): Unit =
    bbuf.putInt( t.length )
    t.foreach( one => elementEngine.write(bbuf, one) )

  def read( bbuf: ByteBuffer ): Array[T] =
    val len = bbuf.getInt()
    (0 to len-1).map(_ => elementEngine.read(bbuf)).toArray


case class MapByteEngine[K:ClassTag, V:ClassTag](keyEngine: BytesEngine[K], valueEngine: BytesEngine[V]) extends BytesEngine[Map[K,V]]:
  def write( bbuf: ByteBuffer, t: Map[K,V] ): Unit =
    bbuf.putInt( t.size )
    t.foreach{ (k,v) => 
      keyEngine.write(bbuf,k) 
      valueEngine.write(bbuf,v) 
    }

  def read( bbuf: ByteBuffer ): Map[K,V] =
    val len = bbuf.getInt()
    (0 to len-1).map{ _ => 
      val k = keyEngine.read(bbuf)
      val v = valueEngine.read(bbuf)
      (k,v)
    }.toMap


case class OptionByteEngine[T](elementEngine: BytesEngine[T]) extends BytesEngine[Option[T]]:
  def write( bbuf: ByteBuffer, t: Option[T] ): Unit =
    if t.isDefined then 
      bbuf.put(1.toByte)
      elementEngine.write(bbuf, t.get)
    else
      bbuf.put(0.toByte)

  def read( bbuf: ByteBuffer ): Option[T] =
    bbuf.get() match {
      case 0 => None
      case 1 => Some( elementEngine.read(bbuf) )
    }
  

// This one's gonna be slow, but how else are we gonna serialize a Method?
object ObjectByteEngine extends BytesEngine[Object]:
  def write( bbuf: ByteBuffer, t: Object ): Unit =
    val baos = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream( baos )
    oos.writeObject( t )
    oos.close()
    val array = baos.toByteArray()
    bbuf.putInt( array.length )
    bbuf.put(array)

  def read( bbuf: ByteBuffer ): Object =
    val len = bbuf.getInt()
    val arrayBuf = new Array[Byte](len)
    bbuf.get(arrayBuf, bbuf.position, len)
    val ois = new ObjectInputStream( new ByteArrayInputStream( arrayBuf ) )
    val o   = ois.readObject()
    ois.close()
    o
