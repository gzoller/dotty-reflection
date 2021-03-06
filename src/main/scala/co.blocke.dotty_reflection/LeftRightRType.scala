package co.blocke.dotty_reflection

import impl._
import info._
import scala.tasty.Reflection

/** Marker trait for all Scala/Java left/right types (either, intersection, union) */
trait LeftRightRType extends AppliedRType:
  self: RType =>

  lazy val leftType: RType
  lazy val rightType: RType

  override def toType(reflect: Reflection): reflect.Type = 
    import reflect.{_, given _}
    AppliedType(Type(self.infoClass), List(leftType.toType(reflect), rightType.toType(reflect)))

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

  override def isAppliedType: Boolean = 
    (leftType match {
      case artL: AppliedRType if artL.isAppliedType => true
      case _ => false
    }) | (rightType match {
      case artR: AppliedRType if artR.isAppliedType => true
      case _ => false
    })

  override def resolveTypeParams( paramMap: Map[TypeSymbol, RType] ): RType = 
    val stage1 = leftType match {
      case ts: TypeSymbolInfo if paramMap.contains(ts.name.asInstanceOf[TypeSymbol]) => _copy(paramMap(ts.name.asInstanceOf[TypeSymbol]), rightType)
      case art: AppliedRType if art.isAppliedType => _copy(leftType.resolveTypeParams(paramMap), rightType)
      case _ => this
    }
    rightType match {
      case ts: TypeSymbolInfo if paramMap.contains(ts.name.asInstanceOf[TypeSymbol]) => _copy(stage1.asInstanceOf[LeftRightRType].leftType, paramMap(ts.name.asInstanceOf[TypeSymbol]))
      case art: AppliedRType if art.isAppliedType => _copy(stage1.asInstanceOf[LeftRightRType].leftType, rightType.resolveTypeParams(paramMap))
      case _ => stage1
    }


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