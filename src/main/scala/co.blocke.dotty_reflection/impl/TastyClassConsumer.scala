package co.blocke.dotty_reflection
package impl

import scala.quoted._
import scala.reflect._
import scala.tasty.file._
import scala.tasty.Reflection
import scala.tasty.file.TastyConsumer

class TastyClassConsumer[T](clazz: Class[_], cache: scala.collection.mutable.HashMap[String, ClassInfo]) extends TastyConsumer
  final def apply(reflect: Reflection)(tree: reflect.Tree): Unit =
    import reflect.{given, _}
    inspectClass(clazz.getName, reflect)(tree).get

  def inspectClass(className: String, reflect: Reflection)(tree: reflect.Tree): Option[ClassInfo] =
    import reflect.{given,_}

    // We expect a certain structure:  PackageClause, which contains ClassDef's for target class + companion object
    val foundClasses = tree match {
      case t: reflect.PackageClause =>
        t.stats.map( m => descendInto(reflect)(m) ).find(_.isDefined).get
      case t => 
        None  // not what we expected!
    }
    foundClasses.iterator.to(List).headOption

  private def descendInto(reflect: Reflection)(tree: reflect.Tree): Option[ClassInfo] =
    import reflect.{given,_}
    tree match {
      case t: reflect.ClassDef if !t.name.endsWith("$") =>
        val className = extractPath(reflect)(t.symbol).mkString(".")+"."+t.name
        cache.get(className).orElse{
          val constructor = t.constructor
          val paramz = constructor.paramss
          val fields = paramz.head.zipWithIndex.map{ (valDef, i) =>
            val fieldTypeInfo: ClassInfo | PrimitiveType | TypeSymbol = inspectType(reflect)(valDef.tpt.tpe.asInstanceOf[TypeRef])

            // See if there's default values specified -- look for gonzo method on companion class.  If exists, default value is available.
            val defaultAccessor = fieldTypeInfo match
              case _: TypeSymbol => None
              case _ =>
                scala.util.Try{
                  val companionClazz = Class.forName(className+"$") // This will fail for non-case classes, including Java classes
                  val defaultMethod = companionClazz.getMethod("$lessinit$greater$default$"+(i+1)) // This will fail if there's no default value for this field
                  val const = companionClazz.getDeclaredConstructor()
                  const.setAccessible(true)
                  ()=>defaultMethod.invoke(const.newInstance())
                }.toOption

            FieldInfo(valDef.name, fieldTypeInfo, defaultAccessor)
            }
          val inspected = InspectedClass(className, fields)
          cache.put(className, inspected)
          Some(inspected)
        }
      case _ => None
    }

  private def inspectType(reflect: Reflection)(typeRef: reflect.TypeRef): ClassInfo | PrimitiveType | TypeSymbol = 
    import reflect.{given,_}
    val classSymbol = typeRef.classSymbol.get
    classSymbol.name match {
      case "Boolean" => PrimitiveType.Scala_Boolean
      case "Byte"    => PrimitiveType.Scala_Byte
      case "Char"    => PrimitiveType.Scala_Char
      case "Double"  => PrimitiveType.Scala_Double
      case "Float"   => PrimitiveType.Scala_Float
      case "Int"     => PrimitiveType.Scala_Int
      case "Long"    => PrimitiveType.Scala_Long
      case "Short"   => PrimitiveType.Scala_Short
      case "String"  => PrimitiveType.Scala_String
      case _ =>
        val isTypeParam = typeRef.typeSymbol.flags.is(Flags.Param)   // Is 'T' or a "real" type?  (true if T)
          if(!isTypeParam)
            descendInto(reflect)(classSymbol.tree).get
          else
            typeRef.name.asInstanceOf[TypeSymbol]
    }

  def extractPath(reflect: Reflection)(symbol: reflect.Symbol) : List[String] = {
    import reflect.{given, _}

    val pathArray = symbol.show.split("\\.") // NOTE: this should print w/o colors, inspect afterwards
    pathArray.iterator.slice(0, pathArray.length - 1).toList
  }
