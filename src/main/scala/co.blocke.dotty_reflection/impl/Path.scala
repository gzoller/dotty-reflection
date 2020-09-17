package co.blocke.dotty_reflection
package impl

import info._

trait PathElement:
  def nav( rt: RType ): RType


case class ClassPathElement(name: String, fieldName: String) extends PathElement:
  def nav( rt: RType ): RType = 
    rt match {
      case c: ScalaCaseClassInfo if c.name == name => c.fields.find(_.name == fieldName).map(_.fieldType).getOrElse(UnknownInfo("X"))
      case c: ScalaClassInfo if c.name == name => c.fields.find(_.name == fieldName).map(_.fieldType).getOrElse(UnknownInfo("X"))
      case c => UnknownInfo("X")
    }


case class TraitPathElement(name: String, fieldName: String) extends PathElement:
  def nav( rt: RType ): RType = 
    rt match {
      case t: TraitInfo if t.name == name => t.fields.find(_.name == fieldName).map(_.fieldType).getOrElse(UnknownInfo("X"))
      case c => UnknownInfo("X")
    }


case class OptionPathElement() extends PathElement:
  def nav( rt: RType ): RType = 
    rt match {
      case o: ScalaOptionInfo => o.optionParamType
      case _ => UnknownInfo("X")
    }

case class TryPathElement() extends PathElement:
  def nav( rt: RType ): RType = 
    rt match {
      case o: TryInfo => o.tryType
      case _ => UnknownInfo("X")
    }

case class SeqPathElement() extends PathElement:
  def nav( rt: RType ): RType = 
    rt match {
      case s: CollectionRType => s.elementType
      case _ => UnknownInfo("X")
    }


case class MapKeyPathElement() extends PathElement:
  def nav( rt: RType ): RType = 
    rt match {
      case m: MapLikeInfo => m.elementType
      case m: JavaMapInfo => m.elementType
      case _ => UnknownInfo("X")
    }


case class MapValuePathElement() extends PathElement:
  def nav( rt: RType ): RType = 
    rt match {
      case m: MapLikeInfo => m.elementType2
      case m: JavaMapInfo => m.elementType2
      case _ => UnknownInfo("X")
    }


case class TuplePathElement(idx: Int) extends PathElement:
  def nav( rt: RType ): RType = 
    rt match {
      case t: TupleInfo => t.tupleTypes(idx)
      case _ => UnknownInfo("X")
    }


case class LeftPathElement() extends PathElement:
  def nav( rt: RType ): RType = 
    rt match {
      case e: EitherInfo => e.leftType
      case i: IntersectionInfo => i.leftType
      case u: UnionInfo => u.leftType
      case _ => UnknownInfo("X")
    }

case class RightPathElement() extends PathElement:
  def nav( rt: RType ): RType = 
    rt match {
      case e: EitherInfo => e.rightType
      case i: IntersectionInfo => i.rightType
      case u: UnionInfo => u.rightType
      case _ => UnknownInfo("X")
    }

// TODO: Java Collection support (likely build into existing)
  

case class Path( p: List[PathElement] ):
  // Tail recursion => basically an abortable foldLeft
  def nav( rt: RType, path: List[PathElement] = p ): Option[RType] = 
    rt match {
      case u: UnknownInfo => None
      case _ if path.isEmpty => Some(rt)
      case _ => nav(path.head.nav(rt), path.tail)
    }
  def push( pe: PathElement ): Path = this.copy( p = this.p :+ pe )
