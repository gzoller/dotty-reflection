package co.blocke.dotty_reflection
package extractors

import impl._
import Clazzes._
import info._ 
import scala.tasty.Reflection
import scala.util.Try

case class JavaSetExtractor() extends TypeInfoExtractor[JavaSetInfo]:

  def matches(reflect: Reflection)(symbol: reflect.Symbol): Boolean = 
    Try( Class.forName(symbol.fullName) <:< JSetClazz ).toOption.getOrElse(false)

  def extractInfo(reflect: Reflection)(
    t: reflect.Type, 
    tob: List[reflect.TypeOrBounds], 
    symbol: reflect.Symbol): RType = 
      val clazz = Class.forName(symbol.fullName)
      val elementType = tob.head.asInstanceOf[reflect.Type]
      val isTypeParam = elementType.typeSymbol.flags.is(reflect.Flags.Param)
      val elementRType = 
        if isTypeParam then
          TypeSymbolInfo(tob.head.asInstanceOf[reflect.Type].typeSymbol.name)
        else
          RType.unwindType(reflect)(tob.head.asInstanceOf[reflect.Type])

      JavaSetInfo(
        clazz.getName, 
        elementRType
      )

  def emptyInfo(clazz: Class[_]): JavaSetInfo = 
    JavaSetInfo(
      clazz.getName, 
      TypeSymbolInfo(clazz.getTypeParameters.toList.head.getName)
    )
