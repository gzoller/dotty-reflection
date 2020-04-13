package co.blocke.dotty_reflection
package extractors

import impl._
import impl.Clazzes._
import infos._ 
import scala.tasty.Reflection

case class JavaListExtractor() extends TypeInfoExtractor[JavaListInfo]:

  def matches(clazz: Class[_]): Boolean = clazz <:< JListClazz

  def emptyInfo(clazz: Class[_]): JavaListInfo = 
    JavaListInfo(
      clazz.getName, 
      clazz, 
      clazz.getTypeParameters.map(_.getName.asInstanceOf[TypeSymbol]).toList, 
      PrimitiveType.Java_Int)

  def extractInfo(reflect: Reflection)(
    t: reflect.Type, 
    tob: List[reflect.TypeOrBounds], 
    className: String, 
    clazz: Class[_], 
    typeInspector: ScalaClassInspector): ConcreteType =

    JavaListInfo(
            className, 
            clazz,
            clazz.getTypeParameters.map(_.getName.asInstanceOf[TypeSymbol]).toList, 
            typeInspector.inspectType(reflect)(tob.head.asInstanceOf[reflect.TypeRef]))
