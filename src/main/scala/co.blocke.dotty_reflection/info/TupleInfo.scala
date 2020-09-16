package co.blocke.dotty_reflection
package info

import scala.tasty.Reflection
import Transporter.AppliedRType
import impl.{Path, TuplePathElement}
import java.nio.ByteBuffer


object TupleInfo:
  def fromBytes( bbuf: ByteBuffer ): TupleInfo = 
    TupleInfo(
      StringByteEngine.read(bbuf),
      ArrayByteEngine[Transporter.RType](RTypeByteEngine).read(bbuf)
      )

case class TupleInfo protected[dotty_reflection](
  name: String,
  _tupleTypes: Array[Transporter.RType]
) extends Transporter.RType with Transporter.AppliedRType:

  val fullName: String = name + _tupleTypes.map(_.fullName).toList.mkString("[",",","]")

  lazy val infoClass: Class[_] = Class.forName(name)

  // Elements may be self-referencing, so we need to unwind this...
  lazy val tupleTypes = _tupleTypes.map( _ match {
    case s: SelfRefRType => s.resolve
    case s => s
  })

  override def toType(reflect: Reflection): reflect.Type = 
    import reflect.{_, given _}
    AppliedType(Type(infoClass), tupleTypes.toList.map( _.toType(reflect) ))

  override def isAppliedType: Boolean = 
    _tupleTypes.map{ _ match {
      case artL: Transporter.AppliedRType if artL.isAppliedType => true
      case _ => false
      }}.foldLeft(false)(_ | _)

  override def resolveTypeParams( paramMap: Map[TypeSymbol, Transporter.RType] ): Transporter.RType = 
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

  override def findPaths(findSyms: Map[TypeSymbol,Path], referenceTrait: Option[TraitInfo] = None): (Map[TypeSymbol, Path], Map[TypeSymbol, Path]) = 
    tupleTypes.zipWithIndex.foldLeft( (Map.empty[TypeSymbol,Path], findSyms) ){ (acc, item) =>
      val (foundSoFar, notYetFound) = acc
      item match {
        case (ts:TypeSymbolInfo, i: Int) if notYetFound.contains(ts.name.asInstanceOf[TypeSymbol]) => 
          val sym = ts.name.asInstanceOf[TypeSymbol]
          (foundSoFar + (sym -> notYetFound(sym).push(TuplePathElement(i))), notYetFound - sym)
        case (other: Transporter.RType, i: Int) =>
          val (fsf2, nyf2) = other.findPaths( notYetFound.map( (k,v) => k -> v.push(TuplePathElement(i))) )
          (fsf2 ++ foundSoFar, nyf2)
      }
    }

  def show(tab: Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + s"""(\n${tupleTypes.map(_.show(newTab,name :: seenBefore)).mkString}""" + tabs(tab) + ")\n"

  def toBytes( bbuf: ByteBuffer ): Unit = 
    bbuf.put( TUPLE_INFO )
    StringByteEngine.write(bbuf, name)
    ArrayByteEngine[Transporter.RType](RTypeByteEngine).write(bbuf, _tupleTypes)
