package co.blocke.dotty_reflection
package impl

import model._
import java.lang.reflect.{Type=>JType,_}
import java.lang.annotation.Annotation
import java.beans.{ Introspector, PropertyDescriptor }
import Clazzes._

object JavaClassInspector:
  def inspectClass(c: Class[?], cache: Reflector.CacheType): ConcreteType =
    val annos:List[Annotation] = c.getAnnotations.toList
    val allAnnos = annos.map(a => parseAnno(a)).toMap
    StaticJavaClassInfo(c.getName, parseFields(c), typeParamSymbols(c), allAnnos)


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

  private def inspectType(mainTypeParams: List[TypeVariable[_]], fieldType: JType): ALL_TYPE =
    fieldType match {
      case g: GenericArrayType => 
        JavaArrayInfo(inspectType(mainTypeParams, g.getGenericComponentType))
      case p: ParameterizedType if p.getRawType.isInstanceOf[Class[_]] => 
        p.getRawType.asInstanceOf[Class[_]] match {
          case c if c =:= OptionalClazz =>
            JavaOptionInfo(c.getName, inspectType(mainTypeParams, p.getActualTypeArguments.head))
          case c if c <:< JMapClazz =>
            val args = p.getActualTypeArguments.toList
            JavaMapInfo(c.getName, typeParamSymbols(c), inspectType(mainTypeParams, args(0)), inspectType(mainTypeParams, args(1)))
          case c if c <:< JListClazz =>
            JavaListInfo(c.getName, typeParamSymbols(c), inspectType(mainTypeParams, p.getActualTypeArguments.head))
          case c if c <:< JQueueClazz =>
            JavaQueueInfo(c.getName, typeParamSymbols(c), inspectType(mainTypeParams, p.getActualTypeArguments.head))
          case c if c <:< JSetClazz =>
            JavaSetInfo(c.getName, typeParamSymbols(c), inspectType(mainTypeParams, p.getActualTypeArguments.head))
          case _ =>
            throw new Exception("Boom - unknown parameterized type")
        }
      case v: TypeVariable[_] => 
        v.getName.asInstanceOf[TypeSymbol]
      case w: WildcardType => throw new Exception("Wildcard types not currently supported in reflection library")
      case other if other.isInstanceOf[Class[_]] => 
        other.asInstanceOf[Class[_]] match {
          case c if c =:= BooleanClazz || c =:= booleanClazz || c =:= JBooleanClazz => PrimitiveType.Scala_Boolean
          case c if c =:= ByteClazz || c =:= byteClazz || c =:= JByteClazz          => PrimitiveType.Scala_Byte
          case c if c =:= CharClazz || c =:= charClazz || c =:= JCharacterClazz     => PrimitiveType.Scala_Char
          case c if c =:= DoubleClazz || c =:= doubleClazz || c =:= JDoubleClazz    => PrimitiveType.Scala_Double
          case c if c =:= FloatClazz || c =:= floatClazz || c =:= JFloatClazz       => PrimitiveType.Scala_Float
          case c if c =:= IntClazz || c =:= intClazz || c =:= JIntegerClazz         => PrimitiveType.Scala_Int
          case c if c =:= LongClazz || c =:= longClazz || c =:= JLongClazz          => PrimitiveType.Scala_Long
          case c if c =:= ShortClazz || c =:= shortClazz || c =:= JShortClazz       => PrimitiveType.Scala_Short
          case c if c =:= StringClazz  => PrimitiveType.Scala_String
          case c if c =:= ObjectClazz  => PrimitiveType.Java_Object
          case c if c.isArray => JavaArrayInfo(inspectType(mainTypeParams, c.getComponentType))
          case n if(mainTypeParams contains fieldType) => n.asInstanceOf[TypeSymbol]
          case c => Reflector.reflectOnClass(c)
        }
      case u =>
        throw new Exception("Unknown (2) Java type "+u)
    }

    /*
    fieldType match {
      case _: ParameterizedType =>
        println("   === made it ===")
        val paramType = fieldType.asInstanceOf[ParameterizedType]
        val paramTypeClass = paramType.getRawType.asInstanceOf[Class[_]]
        paramTypeClass match {
          case p if p =:= OptionalClazz =>
            println("Optional: "+paramTypeClass)
            println("   "+paramType.getActualTypeArguments.head.isInstanceOf[Class[_]])
            println(paramType.getActualTypeArguments.head.getClass.getName)
            val optionType = inspectType(mainTypeParams, paramType.getActualTypeArguments.head)
            JavaOptionInfo(paramTypeClass.getName, optionType)
          // case p if p <:< JMapClazz =>
          //   val keyType = inspectType(mainTypeParams, paramType.getActualTypeArguments(0), ???)
          //   val valueType = inspectType(???)
          //   JavaMapInfo(p.getName, ???, keyType, valueType)
        }
        // paramTypeClass.getName match {
        //   case "java.util.Optional" => 
        //   case n => throw new Exception("Unknown type 2 "+n)
        // }
      case _ =>
        fieldClazz match {
          case c if c =:= BooleanClazz || c =:= booleanClazz || c =:= JBooleanClazz => PrimitiveType.Scala_Boolean
          case c if c =:= ByteClazz || c =:= byteClazz || c =:= JByteClazz          => PrimitiveType.Scala_Byte
          case c if c =:= CharClazz || c =:= charClazz || c =:= JCharacterClazz     => PrimitiveType.Scala_Char
          case c if c =:= DoubleClazz || c =:= doubleClazz || c =:= JDoubleClazz    => PrimitiveType.Scala_Double
          case c if c =:= FloatClazz || c =:= floatClazz || c =:= JFloatClazz       => PrimitiveType.Scala_Float
          case c if c =:= IntClazz || c =:= intClazz || c =:= JIntegerClazz         => PrimitiveType.Scala_Int
          case c if c =:= LongClazz || c =:= longClazz || c =:= JLongClazz          => PrimitiveType.Scala_Long
          case c if c =:= ShortClazz || c =:= shortClazz || c =:= JShortClazz       => PrimitiveType.Scala_Short
          case c if c =:= StringClazz  => PrimitiveType.Scala_String
          case c if c =:= ObjectClazz  => PrimitiveType.Java_Object
          case n if(mainTypeParams contains fieldType) => n.asInstanceOf[TypeSymbol]
          case n => throw new Exception("Unknown type 1 "+n+" with class "+fieldClazz.getName)
        }
    }
    */
/*
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
      */