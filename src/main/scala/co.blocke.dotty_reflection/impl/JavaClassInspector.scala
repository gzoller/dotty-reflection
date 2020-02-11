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
          JavaFieldInfo(i,fieldName, PrimitiveType.Scala_String, fieldAnnos, getter, setter, None)
      }.toList
  


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