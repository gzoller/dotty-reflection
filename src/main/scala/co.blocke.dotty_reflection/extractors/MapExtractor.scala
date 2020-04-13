package co.blocke.dotty_reflection
package extractors

import impl._
import Clazzes._
import info._ 
import scala.tasty.Reflection

case class MapExtractor() extends TypeInfoExtractor[MapLikeInfo]:

  def matches(clazz: Class[_]): Boolean = clazz <:< MapClazz

  def emptyInfo(clazz: Class[_]): MapLikeInfo = 
    val params = clazz.getTypeParameters.toList
    MapLikeInfo(
      clazz.getName, 
      clazz, 
      RType(params(0).getName.asInstanceOf[TypeSymbol]),
      RType(params(1).getName.asInstanceOf[TypeSymbol])
      )

  def extractInfo(reflect: Reflection)(
      t: reflect.Type, 
      tob: List[reflect.TypeOrBounds], 
      className: String, 
      clazz: Class[_], 
      typeInspector: ScalaClassInspector
    ): ConcreteType =

    MapLikeInfo(
      className, 
      clazz,
      typeInspector.inspectType(reflect)(tob(0).asInstanceOf[reflect.TypeRef]),
      typeInspector.inspectType(reflect)(tob(1).asInstanceOf[reflect.TypeRef]))
