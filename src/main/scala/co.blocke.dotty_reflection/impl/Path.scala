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
      case _ => None
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


case class SeqPathElement() extends PathElement:
  def nav( rt: Option[RType] ): Option[RType] = 
    rt.flatMap(_ match {
      case s: SeqLikeInfo => Some(s.elementType)
      case _ => None
    })


// trait MapPathElement


case class Path( p: List[PathElement] ):
  def nav( rt: RType ): Option[RType] = 
    p.foldLeft(Some(rt).asInstanceOf[Option[RType]]){ (item, pathElement) => pathElement.nav(item) }
  def push( pe: PathElement ): Path = this.copy( p = this.p :+ pe )
  def pop: Path = this.copy( p = this.p.dropRight(1) )