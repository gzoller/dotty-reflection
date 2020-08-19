package co.blocke.dotty_reflection
package info

import scala.util.Try
import impl._
import scala.tasty.Reflection

case class TryInfo protected[dotty_reflection](
  name: String,
  _tryType: RType
) extends RType:

  val fullName: String = name + "[" + _tryType.fullName  + "]"
  lazy val infoClass: Class[_] = Class.forName(name)
  lazy val tryType: RType = _tryType match {
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

  override def resolveTypeParams( paramMap: Map[TypeSymbol, RType] ): RType = 
    _tryType match {
      case ts: TypeSymbolInfo if paramMap.contains(ts.name.asInstanceOf[TypeSymbol]) => this.copy(_tryType = paramMap(ts.name.asInstanceOf[TypeSymbol]))
      case pt: impl.PrimitiveType => this
      case other => this.copy(_tryType = other.resolveTypeParams(paramMap))
    }

  def show(tab: Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + s"Try of " + tryType.show(newTab,name :: seenBefore,true)
