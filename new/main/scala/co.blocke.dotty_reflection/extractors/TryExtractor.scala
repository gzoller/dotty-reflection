package co.blocke.dotty_reflection
package extractors

import impl._
import Clazzes._
import info._ 
import scala.tasty.Reflection

case class TryExtractor() extends TypeInfoExtractor[TryInfo]:

  def matches(clazz: Class[_]): Boolean = clazz =:= TryClazz

  def emptyInfo(clazz: Class[_]): TryInfo = 
    TryInfo(
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

      TryInfo(
        className,
        clazz,
        typeInspector.inspectType(reflect)(tob.head.asInstanceOf[reflect.TypeRef])
      )