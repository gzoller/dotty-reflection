package co.blocke.dotty_reflection
package model

import java.lang.reflect._
import java.util.Optional

trait OptionInfo extends ConcreteType:
  val optionParamType: ALL_TYPE


case class ScalaOptionInfo(
  name: String,
  optionParamType: ALL_TYPE
) extends OptionInfo:
  val typeParameters = Nil


case class JavaOptionInfo(
  name: String,
  optionParamType: ALL_TYPE
) extends OptionInfo:
  val typeParameters = Nil
