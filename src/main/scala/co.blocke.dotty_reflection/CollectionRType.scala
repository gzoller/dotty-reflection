package co.blocke.dotty_reflection

import impl._
import info._
import scala.tasty.Reflection

/** Marker trait for all Scala/Java collections */
trait CollectionRType extends AppliedRType:
  self: RType =>

  lazy val elementType: RType

  override def findPaths(findSyms: Map[TypeSymbol,Path], referenceTrait: Option[TraitInfo] = None): (Map[TypeSymbol, Path], Map[TypeSymbol, Path]) = 
    elementType match {
      case ts: TypeSymbolInfo if findSyms.contains(ts.name.asInstanceOf[TypeSymbol]) =>
        val sym = ts.name.asInstanceOf[TypeSymbol]
        (Map( ts.name.asInstanceOf[TypeSymbol] -> findSyms(sym).add(Path.SEQ_PATH).lock ), findSyms - sym)
      case other => 
        other.findPaths(findSyms.map( (k,v) => k -> v.add(Path.SEQ_PATH) ))
    }

  def select(i: Int): RType = 
    if i == 0 then
      elementType
    else
      throw new SelectException(s"AppliedType select index $i out of range for ${self.name}")

  def show(tab: Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName 
    + s"($name): "
    + elementType.show(newTab,name :: seenBefore,true)