package co.blocke.dotty_reflection
package extractors

import impl._
import Clazzes._
import info._ 
import scala.tasty.Reflection

case class OptionExtractor() extends TypeInfoExtractor[ScalaOptionInfo]:

  def matches(clazz: Class[_]): Boolean = clazz =:= OptionClazz

  def emptyInfo(clazz: Class[_]): ScalaOptionInfo = 
    ScalaOptionInfo(
      clazz.getName, 
      clazz, 
      RType(clazz.getTypeParameters.toList.head.getName.asInstanceOf[TypeSymbol])
      )

  def extractInfo(reflect: Reflection)(
    t: reflect.Type, 
    tob: List[reflect.TypeOrBounds], 
    className: String, 
    clazz: Class[_], 
    typeInspector: ScalaClassInspector): ConcreteType =

    ScalaOptionInfo(className, clazz, typeInspector.inspectType(reflect)(tob.head.asInstanceOf[reflect.TypeRef]))