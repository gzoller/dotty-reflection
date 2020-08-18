package co.blocke.dotty_reflection
package info

import scala.tasty.Reflection

case class TupleInfo protected[dotty_reflection](
  name: String,
  _tupleTypes: Array[RType]
) extends RType:

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

  override def resolveTypeParams( paramMap: Map[TypeSymbol, RType] ): RType = 
    var needsCopy = false
    val resolvedTupleTypes = _tupleTypes.map( _ match {
        case ts: TypeSymbolInfo if paramMap.contains(ts.name.asInstanceOf[TypeSymbol]) => 
          needsCopy = true
          paramMap(ts.name.asInstanceOf[TypeSymbol])
        case pt: impl.PrimitiveType => 
          pt
        case other => 
          needsCopy = true
          other.resolveTypeParams(paramMap)
      }
    )
    if needsCopy then
      this.copy(_tupleTypes = resolvedTupleTypes)
    else
      this
  
  def show(tab: Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + s"""(\n${tupleTypes.map(_.show(newTab,name :: seenBefore)).mkString}""" + tabs(tab) + ")\n"
