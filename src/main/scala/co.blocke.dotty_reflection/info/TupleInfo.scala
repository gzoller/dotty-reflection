package co.blocke.dotty_reflection
package info

import scala.tasty.Reflection
import java.nio.ByteBuffer
import impl._


object TupleInfo:
  def fromBytes( bbuf: ByteBuffer ): TupleInfo = 
    TupleInfo(
      StringByteEngine.read(bbuf),
      ArrayRTypeByteEngine.read(bbuf)
      )

case class TupleInfo protected[dotty_reflection](
  name: String,
  _tupleTypes: Array[RType]
) extends RType with AppliedRType:

  val fullName: String = name + _tupleTypes.map(_.fullName).toList.mkString("[",",","]")

  lazy val infoClass: Class[_] = Class.forName(name)

  // Elements may be self-referencing, so we need to unwind this...
  lazy val tupleTypes = _tupleTypes.map( _ match {
    case s: SelfRefRType => s.resolve
    case s => s
  })

  override def isAppliedType: Boolean = 
    _tupleTypes.map{ _ match {
      case artL: AppliedRType if artL.isAppliedType => true
      case _ => false
      }}.foldLeft(false)(_ | _)

  override def resolveTypeParams( paramMap: Map[TypeSymbol, RType] ): RType = 
    var needsCopy = false
    val resolvedTupleTypes = _tupleTypes.map( one => one match {
        case ts: TypeSymbolInfo if paramMap.contains(ts.name.asInstanceOf[TypeSymbol]) => 
          needsCopy = true
          paramMap(ts.name.asInstanceOf[TypeSymbol])
        case art: AppliedRType if art.isAppliedType => 
          needsCopy = true
          one.resolveTypeParams(paramMap)
        case t => t
      }
    )
    if needsCopy then
      TupleInfo(name, resolvedTupleTypes)
    else
      this

  def select(i: Int): RType = 
    if i >= 0 && i <= _tupleTypes.size-1 then
      _tupleTypes(i)
    else 
      throw new ReflectException(s"AppliedType select index $i out of range for ${name}")

  def show(tab: Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + s"""(\n${tupleTypes.map(_.show(newTab,name :: seenBefore)).mkString}""" + tabs(tab) + ")\n"

  def toBytes( bbuf: ByteBuffer ): Unit = 
    bbuf.put( TUPLE_INFO )
    StringByteEngine.write(bbuf, name)
    ArrayRTypeByteEngine.write(bbuf, _tupleTypes)
