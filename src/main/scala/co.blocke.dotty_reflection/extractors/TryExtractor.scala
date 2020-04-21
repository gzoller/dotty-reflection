package co.blocke.dotty_reflection
package extractors

import impl._
import Clazzes._
import info._ 
import scala.tasty.Reflection

case class TryExtractor() extends TypeInfoExtractor[TryInfo]:

  def matches(clazz: Class[_]): Boolean = clazz =:= TryClazz

  def emptyInfo(clazz: Class[_], paramMap: Map[TypeSymbol,RType]): TryInfo = 
    val tryParamSymName = clazz.getTypeParameters.toList.head.getName 
    val tryParamType = paramMap.getOrElse(
      tryParamSymName.asInstanceOf[TypeSymbol], 
      TypeSymbolInfo(tryParamSymName)
      )
    TryInfo(
      clazz.getName, 
      clazz, 
      tryParamType
      )
  
  def extractInfo(reflect: Reflection, paramMap: Map[TypeSymbol,RType])(
    t: reflect.Type, 
    tob: List[reflect.TypeOrBounds], 
    className: String, 
    clazz: Class[_], 
    typeInspector: ScalaClassInspectorLike): RType =

      TryInfo(
        className,
        clazz,
        typeInspector.inspectType(reflect, paramMap)(tob.head.asInstanceOf[reflect.TypeRef])
      )