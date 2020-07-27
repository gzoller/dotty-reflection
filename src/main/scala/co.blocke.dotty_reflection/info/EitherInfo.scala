package co.blocke.dotty_reflection
package info

case class EitherInfo protected[dotty_reflection](
  name: String,
  _leftType: RType,
  _rightType: RType
) extends RType: 

  lazy val infoClass: Class[_] = Class.forName(name)

  lazy val leftType: RType = _leftType match {
    case e: SelfRefRType => e.resolve
    case e => e
  }
  lazy val rightType: RType = _rightType match {
    case e: SelfRefRType => e.resolve
    case e => e
  }

  override def resolveTypeParams( paramMap: Map[TypeSymbol, RType] ): RType = 
    var needsCopy = false
    val left = _leftType match {
      case ts: TypeSymbolInfo if paramMap.contains(ts.name.asInstanceOf[TypeSymbol]) => 
        needsCopy = true
        paramMap(ts.name.asInstanceOf[TypeSymbol])
      case pt: impl.PrimitiveType => 
        _leftType
      case other => 
        needsCopy = true
        other.resolveTypeParams(paramMap)
    }
    val right = _rightType match {
      case ts: TypeSymbolInfo if paramMap.contains(ts.name.asInstanceOf[TypeSymbol]) => 
        needsCopy = true
        paramMap(ts.name.asInstanceOf[TypeSymbol])
      case pt: impl.PrimitiveType => 
        _rightType
      case other => 
        needsCopy = true
        other.resolveTypeParams(paramMap)
    }
    if needsCopy then
      this.copy(_leftType = left, _rightType = right)
    else
      this

  def show(tab: Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + "Either:\n"
    + tabs(newTab)+ "left--" + leftType.show(newTab+1,name :: seenBefore,true)
    + tabs(newTab)+ "right--" + rightType.show(newTab+1,name :: seenBefore,true)
