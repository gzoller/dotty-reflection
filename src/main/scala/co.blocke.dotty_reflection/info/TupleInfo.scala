package co.blocke.dotty_reflection
package info

import scala.tasty.Reflection
import Transporter.AppliedRType


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
      this.copy(_tupleTypes = resolvedTupleTypes)
    else
      this
  
  def show(tab: Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + s"""(\n${tupleTypes.map(_.show(newTab,name :: seenBefore)).mkString}""" + tabs(tab) + ")\n"
