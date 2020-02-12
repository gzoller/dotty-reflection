package co.blocke.dotty_reflection
package impl

import model._
import java.lang.annotation.Annotation
import java.beans.{ Introspector, PropertyDescriptor }

object JavaClassInspector
  def inspectClass(c: Class[?], cache: scala.collection.mutable.HashMap[String, ReflectedThing]): StaticJavaClassInfo =
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
      .zipWithIndex
      .collect { 
        case (desc,i) if desc.getReadMethod() != null && desc.getWriteMethod() != null =>
          val getter = desc.getReadMethod()
          val setter = desc.getWriteMethod()
          val getterAnnos = getter.getAnnotations.map(a => parseAnno(a)).toMap
          val setterAnnos = setter.getAnnotations.map(a => parseAnno(a)).toMap
          val fieldAnnos = getterAnnos ++ setterAnnos
          val fieldName = s"${setter.getName.charAt(3).toLower}${setter.getName.drop(4)}"
          JavaFieldInfo(i,fieldName, inspectType(clazz, fieldName, desc.getPropertyType), fieldAnnos, getter, setter, None)
      }.toList
  
  private def inspectType(mainClass: Class[_], fieldName: String, fieldClass: Class[_]): ALL_TYPE = 
    fieldClass.getName match {
        case "boolean" | "java.lang.Boolean" => PrimitiveType.Scala_Boolean
        case "byte" | "java.lang.Byte"       => PrimitiveType.Scala_Byte
        case "char" | "java.lang.Character"  => PrimitiveType.Scala_Char
        case "double" | "java.lang.Double"   => PrimitiveType.Scala_Double
        case "float" | "java.lang.Float"     => PrimitiveType.Scala_Float
        case "int" | "java.lang.Integer"     => PrimitiveType.Scala_Int
        case "long" | "java.lang.Long"       => PrimitiveType.Scala_Long
        case "short" | "java.lang.Short"     => PrimitiveType.Scala_Short
        case "java.lang.String"  => PrimitiveType.Scala_String
        case "java.lang.Object" =>
          val daField = mainClass.getDeclaredField(fieldName)
          daField.setAccessible(true)
          if(mainClass.getTypeParameters.toList.contains(daField.getGenericType))
            daField.getGenericType.toString.asInstanceOf[TypeSymbol]
          else throw new Exception("Java Object type not yet supported")
        case _ =>  Reflector.reflectOnClass(fieldClass)
      }


      /*

case class FieldInfo(
  index: Int,
  name: String,
  fieldType: ALL_TYPE,
  annotations: Map[String,Map[String,String]],
  valueAccessor: Method,
  defaultValueAccessor: Option[()=>Object]
) extends FieldInfoBase


    case class StaticJavaClassInfo protected (
  val name: String,
  val fields: List[FieldInfo],
  val typeParameters: List[TypeSymbol],
  val annotations: Map[String, Map[String,String]],
  ) extends ClassInfo {

    def constructWith[T](args: List[Object]): T = null.asInstanceOf[T]

  }

    Tasty Scala Case Classes -- Primary case
    Tasty Scala Non-Case Classes  (Might work just like case classes???)
    Known types  -- catch before reflection (special cases)
      collections
      time
      UUID
      ...
    Non-Tasty Scala Classes -- 
    Java Classes -- use getters/setters to get params

*/