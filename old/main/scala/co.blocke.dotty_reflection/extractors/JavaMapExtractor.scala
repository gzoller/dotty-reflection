package co.blocke.dotty_reflection
package extractors

import impl._
import impl.Clazzes._
import infos._ 
import scala.tasty.Reflection

case class JavaMapExtractor() extends TypeInfoExtractor[JavaMapInfo]:

  def matches(clazz: Class[_]): Boolean = clazz <:< JMapClazz

  def emptyInfo(clazz: Class[_]): JavaMapInfo = 
    val params = clazz.getTypeParameters.toList
    JavaMapInfo(
      clazz.getName, 
      clazz, 
      clazz.getTypeParameters.map(_.getName.asInstanceOf[TypeSymbol]).toList, 
      params(0).getName.asInstanceOf[TypeSymbol],
      params(1).getName.asInstanceOf[TypeSymbol]
      )

  def extractInfo(reflect: Reflection)(
    t: reflect.Type, 
    tob: List[reflect.TypeOrBounds], 
    className: String, 
    clazz: Class[_], 
    typeInspector: ScalaClassInspector): ConcreteType =

    JavaMapInfo(
      className, 
      clazz,
      clazz.getTypeParameters.map(_.getName.asInstanceOf[TypeSymbol]).toList, 
      typeInspector.inspectType(reflect)(tob(0).asInstanceOf[reflect.TypeRef]),
      typeInspector.inspectType(reflect)(tob(1).asInstanceOf[reflect.TypeRef]))
