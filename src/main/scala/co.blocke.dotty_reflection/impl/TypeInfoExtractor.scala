package co.blocke.dotty_reflection
package impl

import model._
import scala.tasty.Reflection

trait TypeInfoExtractor:
  def matches(clazz: Class[_]): Boolean
  def extractInfo(reflect: Reflection)(
    t: reflect.Type, 
    tob: List[reflect.TypeOrBounds], 
    className: String, 
    clazz: Class[_], 
    typeInspector: ScalaClassInspector): ALL_TYPE

