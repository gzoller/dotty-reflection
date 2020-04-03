package co.blocke.dotty_reflection
package extractors

import impl._
import impl.Clazzes._
import infos._ 
import scala.tasty.Reflection

case class ScalaArrayExtractor() extends TypeInfoExtractor[ArrayInfo]:

  def matches(clazz: Class[_]): Boolean = clazz <:< ScalaArrayClazz

  private def mangleArrayClassName(tpe: ConcreteType): String =
    val mangled = tpe match {
      case c: ArrayInfo => mangleArrayClassName(c.elementType.asInstanceOf[ConcreteType])
      case PrimitiveType.Scala_Boolean => "Z"
      case PrimitiveType.Scala_Byte => "B"
      case PrimitiveType.Scala_Char => "C"
      case PrimitiveType.Scala_Double => "D"
      case PrimitiveType.Scala_Float => "F"
      case PrimitiveType.Scala_Int => "I"
      case PrimitiveType.Scala_Long => "J"
      case PrimitiveType.Scala_Short => "S"
      case c => "L" + c.name + ";"
    }
    "[" + mangled

  def emptyInfo(clazz: Class[_]): ArrayInfo = ArrayInfo("[I", clazz, PrimitiveType.Scala_Int)

  def extractInfo(reflect: Reflection)(
    t: reflect.Type, 
    tob: List[reflect.TypeOrBounds], 
    className: String, 
    clazz: Class[_], 
    typeInspector: ScalaClassInspector): ConcreteType =

      val elementKind = typeInspector.inspectType(reflect)(tob.head.asInstanceOf[reflect.TypeRef]) match {
        case c: ConcreteType => c
        case c => throw new Exception("Expected concrete type for array element type but got: "+c)
      }
      ArrayInfo(
        mangleArrayClassName(elementKind),
        clazz,
        elementKind)
