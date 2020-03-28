package co.blocke.dotty_reflection
package model

case class TupleInfo protected[dotty_reflection](
  name: String,
  infoClass: Class[_],
  tupleTypes: List[ALL_TYPE]
) extends ConcreteType:
  val typeParameters = infoClass.getTypeParameters.toList.map(_.getName.asInstanceOf[TypeSymbol])
  override def sewTypeParams(actualTypeMap: Map[TypeSymbol, ALL_TYPE]): ConcreteType = 
    this.copy(tupleTypes = tupleTypes.map(_ match {
      case ts: TypeSymbol if actualTypeMap.contains(ts) => actualTypeMap(ts)
      case ts: TypeSymbol => this
      case c: ConcreteType => c.sewTypeParams(actualTypeMap)
  }))
