package co.blocke.dotty_reflection
package extractors

import impl._
import Clazzes._
import info._ 
import scala.tasty.Reflection
import scala.util.Try

case class JavaMapExtractor() extends TypeInfoExtractor[JavaMapInfo]:

  def matches(reflect: Reflection)(symbol: reflect.Symbol): Boolean = 
    Try( Class.forName(symbol.fullName) <:< JMapClazz ).toOption.getOrElse(false)

  def extractInfo(reflect: Reflection)(
    t: reflect.Type, 
    tob: List[reflect.TypeOrBounds], 
    symbol: reflect.Symbol): Transporter.RType = 
      val clazz = Class.forName(symbol.fullName)

      val leftType = tob(0).asInstanceOf[reflect.Type]
      val leftRType = 
        if leftType.typeSymbol.flags.is(reflect.Flags.Param) then
          TypeSymbolInfo(tob(0).asInstanceOf[reflect.Type].typeSymbol.name)
        else
          RType.unwindType(reflect)(tob(0).asInstanceOf[reflect.Type])
  
      val rightType = tob(1).asInstanceOf[reflect.Type]
      val rightRType = 
        if rightType.typeSymbol.flags.is(reflect.Flags.Param) then
          TypeSymbolInfo(tob(1).asInstanceOf[reflect.Type].typeSymbol.name)
        else
          RType.unwindType(reflect)(tob(1).asInstanceOf[reflect.Type])
  
      JavaMapInfo(
        clazz.getName,
        leftRType,
        rightRType
      )

  def emptyInfo(clazz: Class[_]): JavaMapInfo = 
    JavaMapInfo(
      clazz.getName, 
      TypeSymbolInfo(clazz.getTypeParameters.toList.apply(0).getName),
      TypeSymbolInfo(clazz.getTypeParameters.toList.apply(1).getName)
    )
