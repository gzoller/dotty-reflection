package co.blocke.dotty_reflection
package extractors

import impl._
import impl.Clazzes._
import model._ 
import scala.tasty.Reflection

case class TryExtractor() extends TypeInfoExtractor[TryInfo]:

  def matches(clazz: Class[_]): Boolean = clazz =:= TryClazz

  def emptyInfo(clazz: Class[_]): TryInfo = ???
  
  def extractInfo(reflect: Reflection)(
    t: reflect.Type, 
    tob: List[reflect.TypeOrBounds], 
    className: String, 
    clazz: Class[_], 
    typeInspector: ScalaClassInspector): ConcreteType =

      TryInfo(
        className,
        clazz,
        clazz.getTypeParameters.toList.map(_.getName.asInstanceOf[TypeSymbol]),
        typeInspector.inspectType(reflect)(tob.head.asInstanceOf[reflect.TypeRef])
      )
