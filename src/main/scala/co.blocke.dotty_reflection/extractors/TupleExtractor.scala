package co.blocke.dotty_reflection
package extractors

import impl._
import impl.Clazzes._
import model._ 
import scala.tasty.Reflection
import scala.util.matching.Regex

case class TupleExtractor() extends TypeInfoExtractor[TupleInfo]:

  val tupleFullName: Regex = """scala.Tuple(\d+)""".r

  def matches(clazz: Class[_]): Boolean = tupleFullName.matches(clazz.getName)

  def emptyInfo(clazz: Class[_]): TupleInfo = ???

  def extractInfo(reflect: Reflection)(
    t: reflect.Type, 
    tob: List[reflect.TypeOrBounds], 
    className: String, 
    clazz: Class[_], 
    typeInspector: ScalaClassInspector): ConcreteType =

    TupleInfo(className, clazz, tob.map(oneType => typeInspector.inspectType(reflect)(oneType.asInstanceOf[reflect.TypeRef])))
