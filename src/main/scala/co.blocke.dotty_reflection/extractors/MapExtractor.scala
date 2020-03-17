package co.blocke.dotty_reflection
package extractors

import impl._
import impl.Clazzes._
import model._ 
import scala.tasty.Reflection

case class MapExtractor() extends TypeInfoExtractor[Collection_A2_Info]:

  def matches(clazz: Class[_]): Boolean = clazz <:< MapClazz

  def emptyInfo(clazz: Class[_]): Collection_A2_Info = 
    val params = clazz.getTypeParameters.toList
    Collection_A2_Info(
      clazz.getName, 
      clazz, 
      params(0).getName.asInstanceOf[TypeSymbol],
      params(1).getName.asInstanceOf[TypeSymbol]
      )

  def extractInfo(reflect: Reflection)(
    t: reflect.Type, 
    tob: List[reflect.TypeOrBounds], 
    className: String, 
    clazz: Class[_], 
    typeInspector: ScalaClassInspector): ConcreteType =

    Collection_A2_Info(
      className, 
      clazz,
      typeInspector.inspectType(reflect)(tob(0).asInstanceOf[reflect.TypeRef]),
      typeInspector.inspectType(reflect)(tob(1).asInstanceOf[reflect.TypeRef]))
