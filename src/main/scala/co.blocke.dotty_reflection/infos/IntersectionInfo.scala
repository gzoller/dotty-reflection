package co.blocke.dotty_reflection
package infos


case class IntersectionInfo protected[dotty_reflection](
  val name: String,
  val leftType: ALL_TYPE,
  val rightType: ALL_TYPE
  ) extends ConcreteType:
    val typeParameters: List[TypeSymbol] = Nil
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
