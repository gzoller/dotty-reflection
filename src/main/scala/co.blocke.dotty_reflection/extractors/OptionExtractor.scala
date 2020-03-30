package co.blocke.dotty_reflection
package extractors

import impl._
import impl.Clazzes._
import infos._ 
import scala.tasty.Reflection

case class OptionExtractor() extends TypeInfoExtractor[OptionInfo]:

  def matches(clazz: Class[_]): Boolean = clazz =:= OptionClazz

  def emptyInfo(clazz: Class[_]): OptionInfo = ScalaOptionInfo(clazz.getName, clazz, clazz.getTypeParameters.toList.head.getName.asInstanceOf[TypeSymbol])

  def extractInfo(reflect: Reflection)(
    t: reflect.Type, 
    tob: List[reflect.TypeOrBounds], 
    className: String, 
    clazz: Class[_], 
    typeInspector: ScalaClassInspector): ConcreteType =

    ScalaOptionInfo(className, clazz, typeInspector.inspectType(reflect)(tob.head.asInstanceOf[reflect.TypeRef]))