package co.blocke.dotty_reflection
package extractors

import impl._
import Clazzes._
import info._ 
import scala.tasty.Reflection
import scala.util.matching.Regex

case class TupleExtractor() extends TypeInfoExtractor[TupleInfo]:

  val tupleFullName: Regex = """scala.Tuple(\d+)""".r

  def matches(reflect: Reflection)(symbol: reflect.Symbol): Boolean = tupleFullName.matches(symbol.fullName)


  def extractInfo(reflect: Reflection)(
    t: reflect.Type, 
    tob: List[reflect.TypeOrBounds], 
    symbol: reflect.Symbol): Transporter.RType =

    val elementTypes = 
      tob.map{ oneTob =>
        val oneTobType = oneTob.asInstanceOf[reflect.Type]
        if oneTobType.typeSymbol.flags.is(reflect.Flags.Param) then
          TypeSymbolInfo(oneTobType.typeSymbol.name)
        else
          RType.unwindType(reflect)(oneTobType)
      }

    TupleInfo(t.classSymbol.get.fullName, elementTypes.toArray)

