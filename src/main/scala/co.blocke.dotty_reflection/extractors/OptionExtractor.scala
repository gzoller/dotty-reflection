package co.blocke.dotty_reflection
package extractors

import impl._
import impl.Clazzes._
import model._ 
import scala.tasty.Reflection

case class OptionExtractor() extends TypeInfoExtractor:
  def matches(clazz: Class[_]): Boolean = clazz =:= OptionClazz

  def extractInfo(reflect: Reflection)(
    t: reflect.Type, 
    tob: List[reflect.TypeOrBounds], 
    className: String, 
    clazz: Class[_], 
    typeInspector: ScalaClassInspector): ALL_TYPE =

    ScalaOptionInfo(className, clazz, typeInspector.inspectType(reflect)(tob.head.asInstanceOf[reflect.TypeRef]))