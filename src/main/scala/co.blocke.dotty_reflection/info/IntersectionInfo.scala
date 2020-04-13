package co.blocke.dotty_reflection
package info


case class IntersectionInfo protected[dotty_reflection](
  val name: String,
  val leftType: RType,
  val rightType: RType
  ) extends ConcreteType:

    val orderedTypeParameters: List[TypeSymbol] = Nil

    val infoClass: Class[_] = Clazzes.AnyClazz

    def show(tab: Int = 0, supressIndent: Boolean = false): String = 
      val newTab = {if supressIndent then tab else tab+1}
      {if(!supressIndent) tabs(tab) else ""} + "Intersection:\n"
      + tabs(newTab)+ "left--" + leftType.show(newTab+1,true)
      + tabs(newTab)+ "right--" + rightType.show(newTab+1,true)
  
    /*
    override def sewTypeParams(actualTypeMap: Map[TypeSymbol, ALL_TYPE]): ConcreteType = 
      val fixedLeft = leftType match {
        case ts: TypeSymbol if actualTypeMap.contains(ts) => actualTypeMap(ts)
        case ts: TypeSymbol => this
        case c: ConcreteType => c.sewTypeParams(actualTypeMap)
      }
      val fixedRight = rightType match {
        case ts: TypeSymbol if actualTypeMap.contains(ts) => actualTypeMap(ts)
        case ts: TypeSymbol => this
        case c: ConcreteType => c.sewTypeParams(actualTypeMap)
      }
      this.copy( leftType = fixedLeft, rightType = fixedRight )
      */
