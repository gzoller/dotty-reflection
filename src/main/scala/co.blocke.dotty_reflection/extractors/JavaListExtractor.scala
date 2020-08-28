package co.blocke.dotty_reflection
package extractors

import impl._
import Clazzes._
import info._ 
import scala.tasty.Reflection
import scala.util.Try

case class JavaListExtractor() extends TypeInfoExtractor[JavaListInfo]:

  def matches(reflect: Reflection)(symbol: reflect.Symbol): Boolean = 
    Try( Class.forName(symbol.fullName) <:< JListClazz ).toOption.getOrElse(false)

  def extractInfo(reflect: Reflection)(
    t: reflect.Type, 
    tob: List[reflect.TypeOrBounds], 
    symbol: reflect.Symbol): Transporter.RType = 
      val clazz = Class.forName(symbol.fullName)
      val listElementType = tob.head.asInstanceOf[reflect.Type]
      val isTypeParam = listElementType.typeSymbol.flags.is(reflect.Flags.Param)
      val listElementRType = 
        if isTypeParam then
          TypeSymbolInfo(tob.head.asInstanceOf[reflect.Type].typeSymbol.name)
        else
          RType.unwindType(reflect)(tob.head.asInstanceOf[reflect.Type])

      JavaListInfo(
        clazz.getName, 
        listElementRType
      )

  def emptyInfo(clazz: Class[_]): JavaListInfo = 
    JavaListInfo(
      clazz.getName, 
      TypeSymbolInfo(clazz.getTypeParameters.toList.head.getName)
    )
