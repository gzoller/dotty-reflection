package co.blocke.dotty_reflection
package extractors

import impl._
import Clazzes._
import info._ 
import scala.tasty.Reflection

case class ScalaArrayExtractor() extends TypeInfoExtractor[ArrayInfo]:

  def matches(clazz: Class[_]): Boolean = clazz <:< ScalaArrayClazz

  private def mangleArrayClassName(tpe: ConcreteType): String =
    val mangled = tpe match {
      case c: ArrayInfo => mangleArrayClassName(c.elementType.concreteType.asInstanceOf[ConcreteType])
      case PrimitiveType.Scala_Boolean => "Z"
      case PrimitiveType.Scala_Byte => "B"
      case PrimitiveType.Scala_Char => "C"
      case PrimitiveType.Scala_Double => "D"
      case PrimitiveType.Scala_Float => "F"
      case PrimitiveType.Scala_Int => "I"
      case PrimitiveType.Scala_Long => "J"
      case PrimitiveType.Scala_Short => "S"
      case PrimitiveType.Scala_Any => "Ljava.lang.Object;"
      case c => "L" + c.name + ";"
    }
    "[" + mangled

  def emptyInfo(clazz: Class[_]): ArrayInfo = ArrayInfo("[java.lang.Object;", clazz, RType(PrimitiveType.Scala_Any))

  def extractInfo(reflect: Reflection)(
      t: reflect.Type, 
      tob: List[reflect.TypeOrBounds], 
      className: String, 
      clazz: Class[_], 
      typeInspector: ScalaClassInspector
    ): ConcreteType =

    val elementKind = typeInspector.inspectType(reflect)(tob.head.asInstanceOf[reflect.TypeRef]) match {
      case t if t.typeParam.isEmpty => t.concreteType
      case _ => PrimitiveType.Scala_Any
    }
    val mangled = mangleArrayClassName(elementKind)
    ArrayInfo(
      mangled,
      Class.forName(mangled),
      typeInspector.inspectType(reflect)(tob.head.asInstanceOf[reflect.TypeRef]))
