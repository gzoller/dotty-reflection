package co.blocke.dotty_reflection
package extractors

import impl._
import impl.Clazzes._
import model._ 
import scala.tasty.Reflection

case class EitherExtractor() extends TypeInfoExtractor:
  def matches(clazz: Class[_]): Boolean = clazz =:= EitherClazz

  def extractInfo(reflect: Reflection)(
    t: reflect.Type, 
    tob: List[reflect.TypeOrBounds], 
    className: String, 
    clazz: Class[_], 
    typeInspector: ScalaClassInspector): ALL_TYPE =
      
      ScalaEitherInfo(
        className,
        typeInspector.inspectType(reflect)(tob(0).asInstanceOf[reflect.TypeRef]),
        typeInspector.inspectType(reflect)(tob(1).asInstanceOf[reflect.TypeRef])
      )