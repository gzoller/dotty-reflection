package co.blocke.dotty_reflection
package extractors

import impl._
import Clazzes._
import info._ 
import scala.tasty.Reflection

case class ScalaArrayExtractor() extends TypeInfoExtractor[ArrayInfo]:

  def matches(clazz: Class[_]): Boolean = clazz <:< ScalaArrayClazz

  private def mangleArrayClassName(tpe: RType): String =
    val mangled = tpe match {
      case _: TypeSymbolInfo => "Ljava.lang.Object;"
      case c: ArrayInfo => mangleArrayClassName(c.elementType)
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

  def emptyInfo(clazz: Class[_], paramMap: Map[TypeSymbol,RType]): ArrayInfo = 
    val elemParamSymName = clazz.getTypeParameters.toList.head.getName 
    val elemParamType = paramMap.getOrElse(
      elemParamSymName.asInstanceOf[TypeSymbol], 
      TypeSymbolInfo(elemParamSymName)
      )
    ArrayInfo("[java.lang.Object;", clazz, elemParamType)

  def extractInfo(reflect: Reflection, paramMap: Map[TypeSymbol,RType])(
      t: reflect.Type, 
      tob: List[reflect.TypeOrBounds], 
      className: String, 
      clazz: Class[_], 
      typeInspector: ScalaClassInspectorLike
    ): RType =

    val elementType = typeInspector.inspectType(reflect, paramMap)(tob.head.asInstanceOf[reflect.TypeRef])
    val mangled = mangleArrayClassName(elementType)
    ArrayInfo(
      mangled,
      Class.forName(mangled),
      elementType)
