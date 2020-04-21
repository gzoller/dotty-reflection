package co.blocke.dotty_reflection
package extractors

import impl._
import Clazzes._
import info._ 
import scala.tasty.Reflection

case class SeqExtractor() extends TypeInfoExtractor[SeqLikeInfo]:

  def matches(clazz: Class[_]): Boolean = clazz <:< SeqClazz || clazz <:< SetClazz

  def emptyInfo(clazz: Class[_], paramMap: Map[TypeSymbol,RType]): SeqLikeInfo = 
    val elemParamSymName = clazz.getTypeParameters.toList.head.getName 
    val elemParamType = paramMap.getOrElse(
      elemParamSymName.asInstanceOf[TypeSymbol], 
      TypeSymbolInfo(elemParamSymName)
      )
    SeqLikeInfo(
      clazz.getName, 
      clazz, 
      elemParamType
      )

  def extractInfo(reflect: Reflection, paramMap: Map[TypeSymbol,RType])(
    t: reflect.Type, 
    tob: List[reflect.TypeOrBounds], 
    className: String, 
    clazz: Class[_], 
    typeInspector: ScalaClassInspectorLike): RType =

    SeqLikeInfo(
            className, 
            clazz,
            typeInspector.inspectType(reflect, paramMap)(tob.head.asInstanceOf[reflect.TypeRef]))
