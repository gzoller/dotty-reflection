package co.blocke.dotty_reflection
package extractors

import impl._
import Clazzes._
import info._ 
import scala.tasty.Reflection

case class EitherExtractor() extends TypeInfoExtractor[EitherInfo]:

  def matches(clazz: Class[_]): Boolean = clazz =:= EitherClazz

  def emptyInfo(clazz: Class[_], paramMap: Map[TypeSymbol,RType]): EitherInfo = 
    val eitherTypeSyms = clazz.getTypeParameters.toList.map(_.getName)
    val eitherParamTypes = eitherTypeSyms.map( et => paramMap.getOrElse(
      et.asInstanceOf[TypeSymbol], 
      TypeSymbolInfo(et)
      ))
    EitherInfo(clazz.getName, clazz, eitherParamTypes(0), eitherParamTypes(1))

  def extractInfo(reflect: Reflection, paramMap: Map[TypeSymbol,RType])(
    t: reflect.Type, 
    tob: List[reflect.TypeOrBounds], 
    className: String, 
    clazz: Class[_], 
    typeInspector: ScalaClassInspectorLike): RType =
      
      EitherInfo(
        className,
        clazz,
        typeInspector.inspectType(reflect, paramMap)(tob(0).asInstanceOf[reflect.TypeRef]),
        typeInspector.inspectType(reflect, paramMap)(tob(1).asInstanceOf[reflect.TypeRef])
      )