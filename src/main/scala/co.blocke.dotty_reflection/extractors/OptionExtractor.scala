package co.blocke.dotty_reflection
package extractors

import impl._
import Clazzes._
import info._ 
import scala.tasty.Reflection

case class OptionExtractor() extends TypeInfoExtractor[ScalaOptionInfo]:

  def matches(clazz: Class[_]): Boolean = clazz =:= OptionClazz

  def emptyInfo(clazz: Class[_], paramMap: Map[TypeSymbol,RType]): ScalaOptionInfo =
    val optionParamSymName = clazz.getTypeParameters.toList.head.getName 
    val optionParamType = paramMap.getOrElse(
      optionParamSymName.asInstanceOf[TypeSymbol], 
      TypeSymbolInfo(optionParamSymName)
      )
    ScalaOptionInfo(
      clazz.getName, 
      clazz,
      optionParamType
      )

  def extractInfo(reflect: Reflection, paramMap: Map[TypeSymbol,RType])(
    t: reflect.Type, 
    tob: List[reflect.TypeOrBounds], 
    className: String, 
    clazz: Class[_], 
    typeInspector: ScalaClassInspectorLike): RType =

    ScalaOptionInfo(className, clazz, typeInspector.inspectType(reflect, paramMap)(tob.head.asInstanceOf[reflect.TypeRef]))