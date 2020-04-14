package co.blocke.dotty_reflection
package info

case class EitherInfo protected[dotty_reflection](
  name: String,
  infoClass: Class[_],
  leftType: RType,
  rightType: RType
) extends ConcreteType: 

  val orderedTypeParameters = infoClass.getTypeParameters.toList.map(_.getName.asInstanceOf[TypeSymbol])

  def resolveTypeParams(actualTypeMap: Map[TypeSymbol, RType]): ConcreteType = this /* TODO */

  def show(tab: Int = 0, supressIndent: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + "Either:\n"
    + tabs(newTab)+ "left--" + leftType.show(newTab+1,true)
    + tabs(newTab)+ "right--" + rightType.show(newTab+1,true)
    
    /*
  override def sewTypeParams(actualTypeMap: Map[TypeSymbol, ALL_TYPE]): ConcreteType = 
    val fixedLeft = leftParamType match {
      case ts: TypeSymbol if actualTypeMap.contains(ts) => actualTypeMap(ts)
      case ts: TypeSymbol => this
      case c: ConcreteType => c.sewTypeParams(actualTypeMap)
    }
    val fixedRight = rightParamType match {
      case ts: TypeSymbol if actualTypeMap.contains(ts) => actualTypeMap(ts)
      case ts: TypeSymbol => this
      case c: ConcreteType => c.sewTypeParams(actualTypeMap)
    }
    this.copy( leftParamType = fixedLeft, rightParamType = fixedRight )
*/