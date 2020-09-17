package co.blocke.dotty_reflection
package impl

import scala.tasty.Reflection

trait TypeInfoExtractor[T <: RType]:

  def matches(reflect: Reflection)(symbol: reflect.Symbol): Boolean

  def extractInfo(reflect: Reflection)(
    t: reflect.Type, 
    tob: List[reflect.TypeOrBounds], 
    symbol: reflect.Symbol): RType

