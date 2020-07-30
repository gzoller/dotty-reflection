package co.blocke.dotty_reflection
package impl

import info._

trait PathElement:
  def nav( rt: Option[RType] ): Option[RType]


case class ClassPathElement(name: String, fieldName: String) extends PathElement:
  def nav( rt: Option[RType] ): Option[RType] = 
    rt.flatMap(_ match {
      case c: ScalaCaseClassInfo if c.name == name => Some(c)
      case c: ScalaClassInfo if c.name == name => Some(c)
      case c => None
    }).flatMap(_.fields.find(_.name == fieldName).map(_.fieldType))


case class TraitPathElement(name: String, fieldName: String) extends PathElement:
  def nav( rt: Option[RType] ): Option[RType] = 
    rt.flatMap(_ match {
      case t: TraitInfo if t.name == name => Some(t)
      case _ => None
    }).flatMap(_.fields.find(_.name == fieldName).map(_.fieldType))


case class OptionPathElement() extends PathElement:
  def nav( rt: Option[RType] ): Option[RType] = 
    rt.flatMap(_ match {
      case o: ScalaOptionInfo => Some(o.optionParamType)
      case _ => None
    })

case class TryPathElement() extends PathElement:
  def nav( rt: Option[RType] ): Option[RType] = 
    rt.flatMap(_ match {
      case o: TryInfo => Some(o.tryType)
      case _ => None
    })

case class SeqPathElement() extends PathElement:
  def nav( rt: Option[RType] ): Option[RType] = 
    rt.flatMap(_ match {
      case s: CollectionRType => Some(s.elementType)
      case _ => None
    })


case class MapKeyPathElement() extends PathElement:
  def nav( rt: Option[RType] ): Option[RType] = 
    rt.flatMap(_ match {
      case m: MapLikeInfo => Some(m.elementType)
      case m: JavaMapInfo => Some(m.elementType)
      case _ => None
    })


case class MapValuePathElement() extends PathElement:
  def nav( rt: Option[RType] ): Option[RType] = 
    rt.flatMap(_ match {
      case m: MapLikeInfo => Some(m.elementType2)
      case m: JavaMapInfo => Some(m.elementType2)
      case _ => None
    })


case class TuplePathElement(idx: Int) extends PathElement:
  def nav( rt: Option[RType] ): Option[RType] = 
    rt.flatMap(_ match {
      case t: TupleInfo => Some(t.tupleTypes(idx))
      case _ => None
    })


case class LeftPathElement() extends PathElement:
  def nav( rt: Option[RType] ): Option[RType] = 
    rt.flatMap(_ match {
      case e: EitherInfo => Some(e.leftType)
      case i: IntersectionInfo => Some(i.leftType)
      case u: UnionInfo => Some(u.leftType)
      case _ => None
    })


case class RightPathElement() extends PathElement:
  def nav( rt: Option[RType] ): Option[RType] = 
    rt.flatMap(_ match {
      case e: EitherInfo => Some(e.rightType)
      case i: IntersectionInfo => Some(i.rightType)
      case u: UnionInfo => Some(u.rightType)
      case _ => None
    })

// TODO: Java Collection support (likely build into existing)
  

case class Path( p: List[PathElement] ):
  def nav( rt: RType ): Option[RType] = 
    p.foldLeft(Some(rt).asInstanceOf[Option[RType]]){ (item, pathElement) => pathElement.nav(item) }
  def push( pe: PathElement ): Path = this.copy( p = this.p :+ pe )
