package co.blocke.dotty_reflection
package info

import scala.util.Try
import impl._
import scala.tasty.Reflection
import Transporter.AppliedRType
import java.nio.ByteBuffer


object TryInfo:
  def fromBytes( bbuf: ByteBuffer ): TryInfo =
    TryInfo(
      StringByteEngine.read(bbuf),
      RTypeByteEngine.read(bbuf)
      )

case class TryInfo protected[dotty_reflection](
  name: String,
  _tryType: Transporter.RType
) extends Transporter.RType with Transporter.AppliedRType:

  val fullName: String = name + "[" + _tryType.fullName  + "]"
  lazy val infoClass: Class[_] = Class.forName(name)
  lazy val tryType: Transporter.RType = _tryType match {
    case e: SelfRefRType => e.resolve
    case e => e
  }

  override def toType(reflect: Reflection): reflect.Type = 
    import reflect.{_, given _}
    AppliedType(Type(infoClass), List(tryType.toType(reflect)))

  override def findPaths(findSyms: Map[TypeSymbol,Path], referenceTrait: Option[TraitInfo] = None): (Map[TypeSymbol, Path], Map[TypeSymbol, Path]) = 
    tryType match {
      case ts: TypeSymbolInfo if findSyms.contains(ts.name.asInstanceOf[TypeSymbol]) =>
        val sym = ts.name.asInstanceOf[TypeSymbol]
        (Map( ts.name.asInstanceOf[TypeSymbol] -> findSyms(sym).push(TryPathElement()) ), findSyms - sym)
      case other => 
        other.findPaths(findSyms.map( (k,v) => k -> v.push(TryPathElement()) ))
    }

  def show(tab: Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + s"Try of " + tryType.show(newTab,name :: seenBefore,true)

  override def resolveTypeParams( paramMap: Map[TypeSymbol, Transporter.RType] ): Transporter.RType = 
    _tryType match {
      case ts: TypeSymbolInfo if paramMap.contains(ts.name.asInstanceOf[TypeSymbol]) => TryInfo(name, paramMap(ts.name.asInstanceOf[TypeSymbol]))
      case art: AppliedRType if art.isAppliedType => TryInfo(name, _tryType.resolveTypeParams(paramMap))
      case _ => this
    }

  def toBytes( bbuf: ByteBuffer ): Unit = 
    bbuf.put( TRY_INFO )
    StringByteEngine.write(bbuf, name)
    RTypeByteEngine.write(bbuf, _tryType)