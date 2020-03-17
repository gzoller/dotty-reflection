package co.blocke.dotty_reflection
package extractors

import impl._
import impl.Clazzes._
import model._ 
import scala.tasty.Reflection

case class EitherExtractor() extends TypeInfoExtractor[ScalaEitherInfo]:

  def matches(clazz: Class[_]): Boolean = clazz =:= EitherClazz

  def emptyInfo(clazz: Class[_]): ScalaEitherInfo = 
    val params = clazz.getTypeParameters.toList
    val left = params(0).getName.asInstanceOf[TypeSymbol]
    val right = params(1).getName.asInstanceOf[TypeSymbol]
    ScalaEitherInfo(clazz.getName, clazz, left, right)

  def extractInfo(reflect: Reflection)(
    t: reflect.Type, 
    tob: List[reflect.TypeOrBounds], 
    className: String, 
    clazz: Class[_], 
    typeInspector: ScalaClassInspector): ConcreteType =
      
      ScalaEitherInfo(
        className,
        clazz,
        typeInspector.inspectType(reflect)(tob(0).asInstanceOf[reflect.TypeRef]),
        typeInspector.inspectType(reflect)(tob(1).asInstanceOf[reflect.TypeRef])
      )