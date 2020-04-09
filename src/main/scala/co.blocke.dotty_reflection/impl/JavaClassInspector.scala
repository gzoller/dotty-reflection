package co.blocke.dotty_reflection
package impl

import infos._
import java.lang.reflect.{Type=>JType,_}
import java.lang.annotation.Annotation
import java.beans.{ Introspector, PropertyDescriptor }
import Clazzes._

/** Inspects core Java classes including JavaBeans, basic collections (Map/List/Set/Queue flavors), Optional, and primitive types.
 *  Other "extra" classes, e.g. UUID, woudld be handled by creating & registering a custom TypeInfoExtractor, and would be processed in 
 *  ScalaClassInspector by default.
 */
object JavaClassInspector:
  def inspectClass(c: Class[?]): ConcreteType =
    val annos:List[Annotation] = c.getAnnotations.toList
    val allAnnos = annos.map(a => parseAnno(a)).toMap
    JavaClassInfo(c.getName, c, parseFields(c), typeParamSymbols(c), allAnnos)

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

          JavaFieldInfo(0,fieldName, fieldType, fieldAnnos, getter, setter, fieldType.isInstanceOf[TypeSymbol])
      }.toList.filterNot(_.annotations.contains("co.blocke.dotty_reflection.Ignore")).zipWithIndex.map{
        (f,i) => f.copy(index = i)
      }

  private def inspectType(mainTypeParams: List[TypeVariable[_]], fieldType: JType): ALL_TYPE =
    fieldType match {
      case g: GenericArrayType => 
        // Need classOf[Array[g.getGenericComponentType]]
        JavaArrayInfo(classOf[Array], inspectType(mainTypeParams, g.getGenericComponentType))
      // All this stuff gets triggered if there are Java collections *in a Java class*.  They don't get triggered
      // if we're inspecting a top-level collection, i.e. a Java collection that is a member of a Scala class.
      case p: ParameterizedType if p.getRawType.isInstanceOf[Class[_]] => 
        p.getRawType.asInstanceOf[Class[_]] match {
          case c if c =:= OptionalClazz =>
            JavaOptionalInfo(c.getName, c, inspectType(mainTypeParams, p.getActualTypeArguments.head))
          case c if c <:< JMapClazz =>
            val params = p.getActualTypeArguments.toList
            JavaMapInfo(c.getName, c, typeParamSymbols(c), inspectType(mainTypeParams, params(0)), inspectType(mainTypeParams, params(1)))
          case c if c <:< JListClazz =>
            JavaListInfo(c.getName, c, typeParamSymbols(c), inspectType(mainTypeParams, p.getActualTypeArguments.head))
          case c if c <:< JQueueClazz =>
            JavaQueueInfo(c.getName, c, typeParamSymbols(c), inspectType(mainTypeParams, p.getActualTypeArguments.head))
          case c if c <:< JSetClazz =>
            JavaSetInfo(c.getName, c, typeParamSymbols(c), inspectType(mainTypeParams, p.getActualTypeArguments.head))
          case c =>
            val params = p.getActualTypeArguments.toList
            Reflector.reflectOnClassWithParams(c, params.map(pt => Reflector.reflectOnClass(pt.asInstanceOf[Class[_]])))
        }
      case v: TypeVariable[_] => 
        v.getName.asInstanceOf[TypeSymbol]
      case w: WildcardType => throw new ReflectException("Wildcard types not currently supported in reflection library")
      case other if other.isInstanceOf[Class[_]] => 
        other.asInstanceOf[Class[_]] match {
          case c if c =:= BooleanClazz || c =:= booleanClazz || c =:= JBooleanClazz => PrimitiveType.Java_Boolean
          case c if c =:= ByteClazz || c =:= byteClazz || c =:= JByteClazz          => PrimitiveType.Java_Byte
          case c if c =:= CharClazz || c =:= charClazz || c =:= JCharacterClazz     => PrimitiveType.Java_Char
          case c if c =:= DoubleClazz || c =:= doubleClazz || c =:= JDoubleClazz    => PrimitiveType.Java_Double
          case c if c =:= FloatClazz || c =:= floatClazz || c =:= JFloatClazz       => PrimitiveType.Java_Float
          case c if c =:= IntClazz || c =:= intClazz || c =:= JIntegerClazz         => PrimitiveType.Java_Int
          case c if c =:= LongClazz || c =:= longClazz || c =:= JLongClazz          => PrimitiveType.Java_Long
          case c if c =:= ShortClazz || c =:= shortClazz || c =:= JShortClazz       => PrimitiveType.Java_Short
          case c if c =:= JNumberClazz                                              => PrimitiveType.Java_Number
          case c if c =:= StringClazz  => PrimitiveType.Scala_String
          case c if c =:= ObjectClazz  => PrimitiveType.Java_Object
          case c if c.isArray => JavaArrayInfo(c, inspectType(mainTypeParams, c.getComponentType))
          case n if(mainTypeParams contains fieldType) => n.asInstanceOf[TypeSymbol]
          case c if c.isEnum => JavaEnumInfo(c.getName, c)
          case c => Reflector.reflectOnClass(c)
        }
      case u =>
        throw new ReflectException("Unknown Java type "+u)  // This isn't a Class so we can't use UnknownInfo here
    }