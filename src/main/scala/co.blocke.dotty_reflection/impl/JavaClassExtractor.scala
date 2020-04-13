package co.blocke.dotty_reflection
package impl

import info._
import extractors._
import java.lang.reflect.{Type=>JType,_}
import java.lang.annotation.Annotation
import java.beans.{ Introspector, PropertyDescriptor }
import Clazzes._

/** Inspects core Java classes including JavaBeans, basic collections (Map/List/Set/Queue flavors), Optional, and primitive types.
 *  Other "extra" classes, e.g. UUID, woudld be handled by creating & registering a custom TypeInfoExtractor, and would be processed in 
 *  ScalaClassInspector by default.
 */
object JavaClassInspector:
  def inspectClass(c: Class[?]): RType =
    // We must detect and handle any top-level Java collections or they'll be "dumbed-down" to JavaClassInfo, which isn't what we want.
    // (Not worried about the type parameters of the collections here--they'll be populated in the sewTypeParams() method later)
    c match {
      case z if z =:= OptionalClazz => RType(OptionalExtractor().emptyInfo(z))
      case z if z <:< JListClazz    => RType(JavaListExtractor().emptyInfo(z))
      case z if z <:< JMapClazz     => RType(JavaMapExtractor().emptyInfo(z))
      case z if z <:< JQueueClazz   => RType(JavaQueueExtractor().emptyInfo(z))
      case z if z <:< JSetClazz     => RType(JavaSetExtractor().emptyInfo(z))
      case _ =>
        val annos:List[Annotation] = c.getAnnotations.toList
        val allAnnos = annos.map(a => parseAnno(a)).toMap
        RType(JavaClassInfo(c.getName, c, typeParamSymbols(c), parseFields(c), allAnnos))
    }

  private def parseAnno( annoClass: Annotation): (String,Map[String,String]) = 
    val methods = annoClass.annotationType.getDeclaredMethods.toList.map( m => (m.getName, m.invoke(annoClass).toString)).toMap
    (annoClass.annotationType.getName, methods)

  private def typeParamSymbols(c: Class[_]): List[TypeSymbol] = c.getTypeParameters.toList.map(_.getName.asInstanceOf[TypeSymbol])

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

  private def inspectType(mainTypeParams: List[TypeVariable[_]], fieldType: JType): RType =
    fieldType match {
      case g: GenericArrayType => 
        RType(JavaArrayInfo(classOf[Array], inspectType(mainTypeParams, g.getGenericComponentType)))

        // All this stuff gets triggered if there are Java collections *in a Java class*.  They don't get triggered
      // if we're inspecting a top-level collection, i.e. a Java collection that is a member of a Scala class.
      case p: ParameterizedType if p.getRawType.isInstanceOf[Class[_]] => 
        p.getRawType.asInstanceOf[Class[_]] match {
          case c if c =:= OptionalClazz =>
            RType(JavaOptionalInfo(c.getName, c, inspectType(mainTypeParams, p.getActualTypeArguments.head)))
          case c if c <:< JMapClazz =>
            val params = p.getActualTypeArguments.toList
            RType(JavaMapInfo(c.getName, c, typeParamSymbols(c), inspectType(mainTypeParams, params(0)), inspectType(mainTypeParams, params(1))))
          case c if c <:< JListClazz =>
            RType(JavaListInfo(c.getName, c, typeParamSymbols(c), inspectType(mainTypeParams, p.getActualTypeArguments.head)))
          case c if c <:< JQueueClazz =>
            RType(JavaQueueInfo(c.getName, c, typeParamSymbols(c), inspectType(mainTypeParams, p.getActualTypeArguments.head)))
          case c if c <:< JSetClazz =>
            RType(JavaSetInfo(c.getName, c, typeParamSymbols(c), inspectType(mainTypeParams, p.getActualTypeArguments.head)))
          case c =>
            val params = p.getActualTypeArguments.toList
            Reflector.reflectOnClassWithParams(c, params.map(pt => Reflector.reflectOnClass(pt.asInstanceOf[Class[_]])))
        }
      case v: TypeVariable[_] => 
        RType(v.getName.asInstanceOf[TypeSymbol])
      case w: WildcardType => throw new ReflectException("Wildcard types not currently supported in reflection library")
      case other if other.isInstanceOf[Class[_]] => 
        other.asInstanceOf[Class[_]] match {
          case c if c.isArray => RType(JavaArrayInfo(c, inspectType(mainTypeParams, c.getComponentType)))
          case n if(mainTypeParams contains fieldType) => RType(n.asInstanceOf[TypeSymbol])
          case c if c.isEnum => RType(JavaEnumInfo(c.getName, c))
          case c => Reflector.reflectOnClass(c)
        }
      case u =>
        throw new ReflectException("Unknown Java type "+u)  // This isn't a Class so we can't use UnknownInfo here
    }