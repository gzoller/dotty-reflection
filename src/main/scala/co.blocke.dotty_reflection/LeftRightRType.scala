package co.blocke.dotty_reflection

import impl._
import info._

/** Marker trait for all Scala/Java left/right types (either, intersection, union) */
trait LeftRightRType:
  self: RType =>

  lazy val leftType: RType
  lazy val rightType: RType

  override def findPaths(findSyms: Map[TypeSymbol,Path], referenceTrait: Option[TraitInfo] = None): (Map[TypeSymbol, Path], Map[TypeSymbol, Path]) = 
    val (leftFound, leftUnfound) = leftType match {
      case ts: TypeSymbolInfo if findSyms.contains(ts.name.asInstanceOf[TypeSymbol]) =>
        val sym = ts.name.asInstanceOf[TypeSymbol]
        (Map( ts.name.asInstanceOf[TypeSymbol] -> findSyms(sym).push(LeftPathElement()) ), findSyms - sym)
      case other => 
        other.findPaths(findSyms.map( (k,v) => k -> v.push(LeftPathElement()) ))
    }
    val (rightFound, rightUnfound) = leftType match {
      case ts: TypeSymbolInfo if leftUnfound.contains(ts.name.asInstanceOf[TypeSymbol]) =>
        val sym = ts.name.asInstanceOf[TypeSymbol]
        (Map( ts.name.asInstanceOf[TypeSymbol] -> leftUnfound(sym).push(RightPathElement()) ), findSyms - sym)
      case other => 
        other.findPaths(leftUnfound.map( (k,v) => k -> v.push(RightPathElement()) ))
    }
    (leftFound ++ rightFound, rightUnfound)

  def _copy( left: RType, right: RType ): RType

  override def resolveTypeParams( paramMap: Map[TypeSymbol, RType] ): RType = 
    var needsCopy = false
    val left = leftType match {
      case ts: TypeSymbolInfo if paramMap.contains(ts.name.asInstanceOf[TypeSymbol]) => 
        needsCopy = true
        paramMap(ts.name.asInstanceOf[TypeSymbol])
      case pt: impl.PrimitiveType => 
        leftType
      case other => 
        needsCopy = true
        other.resolveTypeParams(paramMap)
    }
    val right = rightType match {
      case ts: TypeSymbolInfo if paramMap.contains(ts.name.asInstanceOf[TypeSymbol]) => 
        needsCopy = true
        paramMap(ts.name.asInstanceOf[TypeSymbol])
      case pt: impl.PrimitiveType => 
        rightType
      case other => 
        needsCopy = true
        other.resolveTypeParams(paramMap)
    }
    if needsCopy then
      this._copy(left, right)
    else
      this


  def show(tab: Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    val simpleName = this.getClass.getSimpleName match {
      case "EitherInfo"       => "Either"
      case "UnionInfo"        => "Union"
      case "IntersectionInfo" => "Intersection"
    }
    {if(!supressIndent) tabs(tab) else ""} + simpleName+":\n"
    + tabs(newTab)+ "left--" + leftType.show(newTab+1,name :: seenBefore,true)
    + tabs(newTab)+ "right--" + rightType.show(newTab+1,name :: seenBefore,true)  