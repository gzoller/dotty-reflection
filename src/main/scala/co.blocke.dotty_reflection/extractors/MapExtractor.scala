package co.blocke.dotty_reflection
package extractors

import impl._
import Clazzes._
import info._ 
import scala.tasty.Reflection

case class MapExtractor() extends TypeInfoExtractor[MapLikeInfo]:

  def matches(reflect: Reflection)(symbol: reflect.Symbol): Boolean =
    // Try here because non-library symbol won't have a class and will explode.
    scala.util.Try( MapClazz.isAssignableFrom( Class.forName(symbol.fullName) ) ).toOption.getOrElse(false)


  def extractInfo(reflect: Reflection)(
    t: reflect.Type, 
    tob: List[reflect.TypeOrBounds], 
    symbol: reflect.Symbol): RType =

    val tparms = MapClazz.getTypeParameters.toList.map(_.getName.asInstanceOf[TypeSymbol])
    MapLikeInfo(
      t.classSymbol.get.fullName,
      RType.unwindType(reflect)(tob(0).asInstanceOf[reflect.Type]),
      RType.unwindType(reflect)(tob(1).asInstanceOf[reflect.Type]))
