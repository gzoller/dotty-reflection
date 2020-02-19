package co.blocke.dotty_reflection
package impl

import model._
import java.lang.reflect.{Type=>JType,_}
import java.lang.annotation.Annotation
import java.beans.{ Introspector, PropertyDescriptor }

// TODO: an Option[java.lang.Integer]... the Integer part finds its way here to be resolved.
// The front door of inspectClass creates a StaticJavaClassInfo object, when in this case we'd want & expect
// a PrimitiveType

object JavaClassInspector
  def inspectClass(c: Class[?], cache: Reflector.CacheType): StaticJavaClassInfo =
    val annos:List[Annotation] = c.getAnnotations.toList
    val allAnnos = annos.map(a => parseAnno(a)).toMap
    StaticJavaClassInfo(c.getName, parseFields(c), c.getTypeParameters.toList.map(_.getName.asInstanceOf[TypeSymbol]), allAnnos)


  private def parseAnno( annoClass: Annotation): (String,Map[String,String]) = 
    val methods = annoClass.annotationType.getDeclaredMethods.toList.map( m => (m.getName, m.invoke(annoClass).toString)).toMap
    (annoClass.annotationType.getName, methods)


  private def parseFields(clazz: Class[?]): List[JavaFieldInfo] = 
    Introspector
      .getBeanInfo(clazz)
      .getPropertyDescriptors
      .toList
      .filterNot(_.getName == "class")
      .collect { 
        case desc if desc.getReadMethod() != null && desc.getWriteMethod() != null =>
          val getter = desc.getReadMethod()
          val setter = desc.getWriteMethod()
          val getterAnnos = getter.getAnnotations.map(a => parseAnno(a)).toMap
          val setterAnnos = setter.getAnnotations.map(a => parseAnno(a)).toMap
          val fieldAnnos = getterAnnos ++ setterAnnos
          val fieldName = s"${setter.getName.charAt(3).toLower}${setter.getName.drop(4)}"
          val fieldType = inspectType(clazz.getTypeParameters.toList, getter.getGenericReturnType)

          JavaFieldInfo(0,fieldName, fieldType, fieldAnnos, getter, setter)
      }.toList.filterNot(_.annotations.contains("co.blocke.dotty_reflection.Ignore")).zipWithIndex.map{
        (f,i) => f.copy(index = i)
      }

  private def inspectType(mainTypeParams: List[TypeVariable[_]], fieldType: JType): ALL_TYPE =
    if !fieldType.isInstanceOf[ParameterizedType] then
      fieldType.getTypeName match {
        case "boolean" | "java.lang.Boolean" => PrimitiveType.Scala_Boolean
        case "byte" | "java.lang.Byte"       => PrimitiveType.Scala_Byte
        case "char" | "java.lang.Character"  => PrimitiveType.Scala_Char
        case "double" | "java.lang.Double"   => PrimitiveType.Scala_Double
        case "float" | "java.lang.Float"     => PrimitiveType.Scala_Float
        case "int" | "java.lang.Integer"     => PrimitiveType.Scala_Int
        case "long" | "java.lang.Long"       => PrimitiveType.Scala_Long
        case "short" | "java.lang.Short"     => PrimitiveType.Scala_Short
        case "java.lang.String"              => PrimitiveType.Scala_String
        case "java.lang.Object"              => PrimitiveType.Java_Object
        case n if(mainTypeParams contains fieldType) => n.asInstanceOf[TypeSymbol]
        case n => throw new Exception("Unknown type 1 "+n)
      }
    else
      val paramType = fieldType.asInstanceOf[ParameterizedType]
      val paramTypeClass = paramType.getRawType.asInstanceOf[Class[_]]
      paramTypeClass.getName match {
        case "java.util.Optional" => 
          val optionType = inspectType(mainTypeParams, paramType.getActualTypeArguments.head)
          JavaOptionInfo(paramTypeClass.getName, optionType)
        case n => throw new Exception("Unknown type 2 "+n)
      }