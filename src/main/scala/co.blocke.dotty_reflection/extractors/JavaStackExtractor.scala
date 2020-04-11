package co.blocke.dotty_reflection
package extractors

import impl._
import impl.Clazzes._
import infos._ 
import scala.tasty.Reflection

case class JavaStackExtractor() extends TypeInfoExtractor[JavaStackInfo]:

  def matches(clazz: Class[_]): Boolean = clazz <:< JStackClazz

  def emptyInfo(clazz: Class[_]): JavaStackInfo = 
    JavaStackInfo(
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

      JavaStackInfo(
          className, 
          clazz,
          clazz.getTypeParameters.map(_.getName.asInstanceOf[TypeSymbol]).toList, 
          typeInspector.inspectType(reflect)(tob.head.asInstanceOf[reflect.TypeRef]))