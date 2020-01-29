package co.blocke.dotty_reflection
package impl

import scala.quoted._
import scala.reflect._
import scala.tasty.file._
import scala.tasty.Reflection
import scala.tasty.file.TastyConsumer

class TastyClassConsumer[T](clazz: Class[_], cache: scala.collection.mutable.HashMap[String, ReflectedThing]) extends TastyConsumer
  final def apply(reflect: Reflection)(tree: reflect.Tree): Unit =
    import reflect.{given, _}
    inspectClass(clazz.getName, reflect)(tree).get

  def inspectClass(className: String, reflect: Reflection)(tree: reflect.Tree): Option[ReflectedThing] =
    import reflect.{given,_}

    // We expect a certain structure:  PackageClause, which contains ClassDef's for target class + companion object
    val foundClasses = tree match {
      case t: reflect.PackageClause =>
        t.stats.map( m => descendInto(reflect)(m) ).find(_.isDefined).get
      case t => 
        None  // not what we expected!
    }
    foundClasses.iterator.to(List).headOption

  private def descendInto(reflect: Reflection)(tree: reflect.Tree): Option[ReflectedThing] =
    import reflect.{given,_}
    tree match {
      case t: reflect.ClassDef if !t.name.endsWith("$") =>
        val className = t.symbol.show
        cache.get(className).orElse{
          val constructor = t.constructor
          val typeParams = constructor.typeParams.map(x => x.show.stripPrefix("type ")).map(_.toString.asInstanceOf[TypeSymbol])
          val inspected = if(t.symbol.flags.is(Flags.Trait))
            StaticTraitInfo(className, typeParams)
          else
            val paramz = constructor.paramss
            val fields = paramz.head.zipWithIndex.map( (valDef, i) => inspectField(reflect)(valDef, i, className) )
            StaticClassInfo(className, fields, typeParams)
          cache.put(className, inspected)
          Some(inspected)
        }
      case _ => None
    }

  private def inspectField(reflect: Reflection)(valDef: reflect.ValDef, index: Int, className: String): FieldInfo =
    import reflect.{given,_}
    val fieldTypeInfo: ReflectedThing | PrimitiveType | TypeSymbol = inspectType(reflect)(valDef.tpt.tpe.asInstanceOf[TypeRef])

    // See if there's default values specified -- look for gonzo method on companion class.  If exists, default value is available.
    val defaultAccessor = fieldTypeInfo match
      case _: TypeSymbol => None
      case _ =>
        scala.util.Try{
          val companionClazz = Class.forName(className+"$") // This will fail for non-case classes, including Java classes
          val defaultMethod = companionClazz.getMethod("$lessinit$greater$default$"+(index+1)) // This will fail if there's no default value for this field
          val const = companionClazz.getDeclaredConstructor()
          const.setAccessible(true)
          ()=>defaultMethod.invoke(const.newInstance())
        }.toOption
    val valueAccessor = Class.forName(className).getDeclaredMethod(valDef.name)
    FieldInfo(valDef.name, fieldTypeInfo, valueAccessor, defaultAccessor)

  private def inspectType(reflect: Reflection)(typeRef: reflect.TypeRef): ReflectedThing | PrimitiveType | TypeSymbol = 
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