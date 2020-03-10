package co.blocke.dotty_reflection
package model


case class StaticIntersectionInfo protected (
  val name: String,
  val typeParameters: List[TypeSymbol],
  val leftType: ALL_TYPE,
  val rightType: ALL_TYPE
  ) extends ConcreteType:
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
