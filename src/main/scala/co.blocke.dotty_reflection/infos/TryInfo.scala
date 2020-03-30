package co.blocke.dotty_reflection
package infos

import scala.util.Try

case class TryInfo protected[dotty_reflection](
  name: String,
  infoClass: Class[_],
  tryType: ALL_TYPE
) extends ConcreteType:
  val typeParameters = infoClass.getTypeParameters.toList.map(_.getName.asInstanceOf[TypeSymbol])
  override def sewTypeParams(actualTypeMap: Map[TypeSymbol, ALL_TYPE]): ConcreteType = tryType match {
    case ts: TypeSymbol if actualTypeMap.contains(ts) => this.copy(tryType = actualTypeMap(ts))
    case ts: TypeSymbol => this
    case c: ConcreteType => this.copy(tryType = c.sewTypeParams(actualTypeMap))
  }
