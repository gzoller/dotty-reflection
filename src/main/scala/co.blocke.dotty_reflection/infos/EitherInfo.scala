package co.blocke.dotty_reflection
package infos

case class EitherInfo protected[dotty_reflection](
  name: String,
  infoClass: Class[_],
  leftParamType: ALL_TYPE,
  rightParamType: ALL_TYPE
) extends ConcreteType: 

  val typeParameters = infoClass.getTypeParameters.toList.map(_.getName.asInstanceOf[TypeSymbol])

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
