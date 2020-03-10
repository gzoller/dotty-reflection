package co.blocke.dotty_reflection
package model

case class ScalaEitherInfo(
  name: String,
  leftParamType: ALL_TYPE,
  rightParamType: ALL_TYPE
) extends ConcreteType: 
  val typeParameters = Nil
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
