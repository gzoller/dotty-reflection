package co.blocke.dotty_reflection
package extractors

import impl._
import Clazzes._
import info._ 
import scala.tasty.Reflection

case class OptionalExtractor() extends TypeInfoExtractor[JavaOptionalInfo]:

  def matches(clazz: Class[_]): Boolean = clazz =:= OptionalClazz

  def emptyInfo(clazz: Class[_], paramMap: Map[TypeSymbol,RType]): JavaOptionalInfo = 
    val optionParamSymName = clazz.getTypeParameters.toList.head.getName 
    val optionParamType = paramMap.getOrElse(
      optionParamSymName.asInstanceOf[TypeSymbol], 
      TypeSymbolInfo(optionParamSymName)
      )
    JavaOptionalInfo(
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

    JavaOptionalInfo(className, clazz, typeInspector.inspectType(reflect, paramMap)(tob.head.asInstanceOf[reflect.TypeRef]))
