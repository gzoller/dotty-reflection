package co.blocke.dotty_reflection
package extractors

import impl._
import Clazzes._
import info._ 
import scala.tasty.Reflection

case class EitherExtractor() extends TypeInfoExtractor[EitherInfo]:

  def matches(reflect: Reflection)(symbol: reflect.Symbol): Boolean = symbol.fullName == EitherClazz.getName


  def extractInfo(reflect: Reflection)(
    t: reflect.Type, 
    tob: List[reflect.TypeOrBounds], 
    symbol: reflect.Symbol): RType =

    val tparms = EitherClazz.getTypeParameters.toList.map(_.getName.asInstanceOf[TypeSymbol])
    EitherInfo(
      t.classSymbol.get.fullName,
      RType.unwindType(reflect)(tob(0).asInstanceOf[reflect.Type]),
      RType.unwindType(reflect)(tob(1).asInstanceOf[reflect.Type])
    )
