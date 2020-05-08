package co.blocke.dotty_reflection
package impl

import info._
import scala.tasty.Reflection

trait TypeInfoExtractor[T <: RType]:

  def matches(clazz: Class[_]): Boolean

  def emptyInfo(clazz: Class[_], paramMap: Map[TypeSymbol,RType]): T

  def extractInfo(reflect: Reflection, paramMap: Map[TypeSymbol,RType])(
    t: reflect.Type, 
    tob: List[reflect.TypeOrBounds], 
    className: String, 
    clazz: Class[_], 
    typeInspector: ScalaClassInspectorLike): RType

