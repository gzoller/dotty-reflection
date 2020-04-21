package co.blocke.dotty_reflection
package extractors

import impl._
import Clazzes._
import info._ 
import scala.tasty.Reflection

case class MapExtractor() extends TypeInfoExtractor[MapLikeInfo]:

  def matches(clazz: Class[_]): Boolean = clazz <:< MapClazz

  def emptyInfo(clazz: Class[_], paramMap: Map[TypeSymbol,RType]): MapLikeInfo = 
    val keyParamSymName = clazz.getTypeParameters()(0).getName 
    val keyParamType = paramMap.getOrElse(
      keyParamSymName.asInstanceOf[TypeSymbol], 
      TypeSymbolInfo(keyParamSymName)
      )
    val valueParamSymName = clazz.getTypeParameters()(1).getName 
    val valueParamType = paramMap.getOrElse(
      valueParamSymName.asInstanceOf[TypeSymbol], 
      TypeSymbolInfo(valueParamSymName)
      )
    MapLikeInfo(
      clazz.getName, 
      clazz, 
      keyParamType,
      valueParamType
      )

  def extractInfo(reflect: Reflection, paramMap: Map[TypeSymbol,RType])(
      t: reflect.Type, 
      tob: List[reflect.TypeOrBounds], 
      className: String, 
      clazz: Class[_], 
      typeInspector: ScalaClassInspectorLike
    ): RType =

    MapLikeInfo(
      className, 
      clazz,
      typeInspector.inspectType(reflect, paramMap)(tob(0).asInstanceOf[reflect.TypeRef]),
      typeInspector.inspectType(reflect, paramMap)(tob(1).asInstanceOf[reflect.TypeRef]))
