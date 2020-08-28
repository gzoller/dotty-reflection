package co.blocke.dotty_reflection
package extractors

import impl._
import Clazzes._
import info._ 
import scala.tasty.Reflection

case class OptionExtractor() extends TypeInfoExtractor[ScalaOptionInfo]:

  def matches(reflect: Reflection)(symbol: reflect.Symbol): Boolean = symbol.fullName == OptionClazz.getName

  
  def extractInfo(reflect: Reflection)(
    t: reflect.Type, 
    tob: List[reflect.TypeOrBounds], 
    symbol: reflect.Symbol): Transporter.RType =

    val optionOfType = tob.head.asInstanceOf[reflect.Type]
    val isTypeParam = optionOfType.typeSymbol.flags.is(reflect.Flags.Param)
    val optionOfRType = 
      if isTypeParam then
        TypeSymbolInfo(tob.head.asInstanceOf[reflect.Type].typeSymbol.name)
      else
        RType.unwindType(reflect)(tob.head.asInstanceOf[reflect.Type])

    ScalaOptionInfo(
      t.classSymbol.get.fullName, 
      optionOfRType
    )
