package co.blocke.dotty_reflection
package impl

import info._
import scala.tasty.Reflection

trait TypeInfoExtractor[T <: ConcreteType]:

  def matches(clazz: Class[_]): Boolean

  def emptyInfo(clazz: Class[_]): T

  def extractInfo(reflect: Reflection)(
    t: reflect.Type, 
    tob: List[reflect.TypeOrBounds], 
    className: String, 
    clazz: Class[_], 
    typeInspector: ScalaClassInspector): ConcreteType

