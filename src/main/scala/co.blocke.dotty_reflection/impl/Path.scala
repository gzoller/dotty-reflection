package co.blocke.dotty_reflection
package impl

import info._
import java.nio.ByteBuffer

object Path:
  val RIGHT_PATH     : Byte = 1
  val LEFT_PATH      : Byte = 2
  val TUPLE_PATH     : Byte = 3  // 1-byte index arg
  val MAP_KEY_PATH   : Byte = 4
  val MAP_VALUE_PATH : Byte = 5
  val SEQ_PATH       : Byte = 6
  val TRY_PATH       : Byte = 7
  val OPTION_PATH    : Byte = 8
  val TRAIT_PATH     : Byte = 9  // 1-byte field index option
  val CLASS_PATH     : Byte = 10 // 1-byte field index option

  // def buildPath: PathBuilder = PathBuilder( ByteBuffer.allocate(512) ) // 512 is arbitrary--large enough for deep/broad nesting
  def buildPath: Path = Path( ByteBuffer.allocate(512) ) // 512 is arbitrary--large enough for deep/broad nesting


case class Path protected ( buf: ByteBuffer ):
  def add( pathElement: Byte, arg: Byte = -1 ): Path =  // WARNING: Mutable state!  Not functional...
    buf.put(pathElement)
    if arg >= 0 then 
      buf.put(arg)
    this

  // Fork for: Map K/V, Tuple fields, class fields, trait fields
  def fork: Path =
    val buf2 = ByteBuffer.wrap(buf.array)
    Path( buf2.position(buf.position()) )

  // Call this to "freeze" (i.e. hard-copy) the path (underlying array)
  // PROBLEM: May be locked or unlocked when toString is called... affects buf.position().  If locked, position() is 0
  def lock: Path = 
    val newBuf = ByteBuffer.wrap(buf.array.slice(0, buf.position()))
    newBuf.position(buf.position())
    Path(newBuf)

  def nav( rt: RType ): RType =
    if !buf.hasRemaining then
      rt
    else
      buf.get() match {
        case Path.RIGHT_PATH      => nav(rt.asInstanceOf[LeftRightRType].rightType)
        case Path.LEFT_PATH       => nav(rt.asInstanceOf[LeftRightRType].leftType)
        case Path.TUPLE_PATH      => nav(rt.asInstanceOf[TupleInfo].tupleTypes(buf.get().toInt))
        case Path.MAP_KEY_PATH    =>
          rt match {
            case j: JavaMapInfo => nav(j.elementType)
            case s: MapLikeInfo => nav(s.elementType)
          }
        case Path.MAP_VALUE_PATH  =>
          rt match {
            case j: JavaMapInfo => nav(j.elementType2)
            case s: MapLikeInfo => nav(s.elementType2)
          }
        case Path.SEQ_PATH        => nav(rt.asInstanceOf[CollectionRType].elementType)
        case Path.TRY_PATH        => nav(rt.asInstanceOf[TryInfo].tryType)
        case Path.OPTION_PATH     => nav(rt.asInstanceOf[OptionInfo].optionParamType)
        case Path.TRAIT_PATH      => nav(rt.asInstanceOf[TraitInfo].fields(buf.get().toInt).fieldType)
        case Path.CLASS_PATH      => nav(rt.asInstanceOf[ClassInfo].fields(buf.get().toInt).fieldType)
      }

  override def toString: String =
    val b = ByteBuffer.wrap(buf.array.slice(0, buf.position()))
    val sb = scala.collection.mutable.StringBuilder("Path ==>\n")
    while b.hasRemaining() do 
      b.get() match {
        case Path.RIGHT_PATH      => sb ++= "   right\n"
        case Path.LEFT_PATH       => sb ++= "   left\n"
        case Path.TUPLE_PATH      => sb ++= s"   tuple(${b.get().toInt})\n"
        case Path.MAP_KEY_PATH    => sb ++= "   map key\n"
        case Path.MAP_VALUE_PATH  => sb ++= "   map value\n"
        case Path.SEQ_PATH        => sb ++= "   seq\n"
        case Path.TRY_PATH        => sb ++= "   tty\n"
        case Path.OPTION_PATH     => sb ++= "   opion\n"
        case Path.TRAIT_PATH      => sb ++= s"   trait field(${b.get().toInt})\n"
        case Path.CLASS_PATH      => sb ++= s"   class field(${b.get().toInt})\n"
      }
    sb.toString
