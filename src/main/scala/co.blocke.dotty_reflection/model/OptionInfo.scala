package co.blocke.dotty_reflection
package model

import java.lang.reflect._
import java.util.Optional

trait OptionInfo extends ConcreteType:
  val optionParamType: ALL_TYPE


case class ScalaOptionInfo(
  name: String,
  infoClass: Class[_],
  optionParamType: ALL_TYPE
) extends OptionInfo:
  val typeParameters = infoClass.getTypeParameters.toList.map(_.getName.asInstanceOf[TypeSymbol])
  override def sewTypeParams(actualTypeMap: Map[TypeSymbol, ALL_TYPE]): ConcreteType = optionParamType match {
    case ts: TypeSymbol if actualTypeMap.contains(ts) => this.copy(optionParamType = actualTypeMap(ts))
    case ts: TypeSymbol => this
    case c: ConcreteType => this.copy(optionParamType = c.sewTypeParams(actualTypeMap))
  }


case class JavaOptionInfo(
  name: String,
  infoClass: Class[_],
  optionParamType: ALL_TYPE
) extends OptionInfo:
  val typeParameters = infoClass.getTypeParameters.toList.map(_.getName.asInstanceOf[TypeSymbol])
  override def sewTypeParams(actualTypeMap: Map[TypeSymbol, ALL_TYPE]): ConcreteType = optionParamType match {
    case ts: TypeSymbol if actualTypeMap.contains(ts) => this.copy(optionParamType = actualTypeMap(ts))
    case ts: TypeSymbol => this
    case c: ConcreteType => this.copy(optionParamType = c.sewTypeParams(actualTypeMap))
  }
