package co.blocke.dotty_reflection
package impl

import info._
import extractors._
import scala.quoted._
import scala.reflect._
import scala.tasty.Reflection
import scala.tasty.inspector.TastyInspector
import Clazzes._

trait ScalaClassInspectorLike:

  def inspectType(reflect: Reflection, paramMap: Map[TypeSymbol,RType])(typeRef: reflect.TypeRef): RType = 
    import reflect.{_, given _}

    typeRef.classSymbol match {

      // Intersection types don't have a class symbol, so don't assume one!
      case None =>
        typeRef match {
          // Intersection Type
          //----------------------------------------
          case AndType(left,right) =>
            val resolvedLeft: RType = inspectType(reflect, paramMap)(left.asInstanceOf[reflect.TypeRef])
            val resolvedRight: RType = inspectType(reflect, paramMap)(right.asInstanceOf[reflect.TypeRef])
            IntersectionInfo(INTERSECTION_CLASS, resolvedLeft, resolvedRight)
          case u => throw new ReflectException("Unsupported TypeRef: "+typeRef)
        }

      case Some(classSymbol) =>
        // Handle gobbled non-class scala.Enumeration.Value (old 2.x Enumeration class values)
        val (is2xEnumeration, className) = classSymbol.fullName match { 
          case raw if raw == ENUM_CLASSNAME => 
            val enumerationClass = typeRef.typeSymbol.fullName
            if( enumerationClass == ENUM_CLASSNAME ) then
              // If caller did NOT defined a type member (type X = Value) inside their Enumeration class
              val enumClassName = typeRef.qualifier.asInstanceOf[reflect.TermRef].termSymbol.moduleClass.fullName.dropRight(1) // chop the '$' off the end!
              (true, enumClassName)
            else
              // If caller defined a type member (type X = Value) inside their Enumeration class
              (true, enumerationClass.dropRight(enumerationClass.length - enumerationClass.lastIndexOf('$')))
          case _  => (false, classSymbol.fullName)
        }

        typeRef match {
          // Scala3 opaque type alias
          //----------------------------------------
          case named: dotty.tools.dotc.core.Types.NamedType if typeRef.isOpaqueAlias =>
            inspectType(reflect, paramMap)(typeRef.translucentSuperType.asInstanceOf[reflect.TypeRef]) match {
              case t: TypeSymbolInfo => throw new ReflectException("Opaque aliases for type symbols currently unsupported")
              case t => AliasInfo(typeRef.show, t)
            }

          // Scala3 Tasty-equipped type incl. primitive types
          //----------------------------------------
          case named: dotty.tools.dotc.core.Types.NamedType => 
            val isTypeParam = typeRef.typeSymbol.flags.is(Flags.Param)   // Is 'T' or a "real" type?  (true if T)
            val anySymbol = Symbol.classSymbol("scala.Any")
            classSymbol match {
              case cs if isTypeParam     => 
                // See if we can resolve the type symbol
                paramMap.get(typeRef.name.asInstanceOf[TypeSymbol]).getOrElse(
                  TypeSymbolInfo(typeRef.name)  // TypeSymbols Foo[T] have typeRef of Any
                  )
              case cs if cs == anySymbol => PrimitiveType.Scala_Any
              case cs =>
                Class.forName(className) match {
                  case c if c <:< EnumClazz => ScalaEnumInfo(className, c)
                  case c if is2xEnumeration => ScalaEnumerationInfo(className, c)
                  case c                    => Reflector.reflectOnClass(c)  // it's some other class, likely a Java or 2.x Scala class
                }
            }

          // Union Type
          //----------------------------------------
          case OrType(left,right) =>
            val resolvedLeft = inspectType(reflect, paramMap)(left.asInstanceOf[reflect.TypeRef])
            val resolvedRight = inspectType(reflect, paramMap)(right.asInstanceOf[reflect.TypeRef])
            UnionInfo(UNION_CLASS, resolvedLeft, resolvedRight)
        
          // Most other "normal" Types
          //----------------------------------------
          case AppliedType(t,tob) => 
            val clazz = Class.forName(className)

            val foundType: Option[RType] = ExtractorRegistry.extractors.collectFirst {
              case e if e.matches(clazz) => e.extractInfo(reflect, paramMap)(t, tob, className, clazz, this)   
            }
            foundType.getOrElse{
              // Some other class we need to descend into, including a parameterized Scala class
              Reflector.reflectOnClassWithParams(clazz, tob.map(typeP => 
                inspectType(reflect, paramMap)(typeP.asInstanceOf[reflect.TypeRef])
              ))
            }
        
          case x => 
            UnknownInfo(Class.forName(className))
        }
    }