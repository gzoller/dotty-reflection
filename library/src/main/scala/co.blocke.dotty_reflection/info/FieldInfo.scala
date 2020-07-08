package co.blocke.dotty_reflection
package info

import java.lang.reflect.Method

trait FieldInfo extends Serializable:
  val index:                Int
  val name:                 String
  val fieldType:            RType
  val originalSymbol:       Option[TypeSymbol]
  val annotations:          Map[String,Map[String,String]]
  lazy val defaultValue:    Option[Object]

  def valueOf[T](target: T): Object
  def reIndex(i: Int): FieldInfo

  def show(tab: Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} 
      + s"(${if !modified then index else '_'})" 
      + {if originalSymbol.isDefined then s"[${originalSymbol.get}]" else ""}
      + s" $name: " 
      + fieldType.show(newTab,name :: seenBefore,true) 
      + { if annotations.nonEmpty then tabs(newTab) + "annotations: " + annotations.toString + "\n" else "" }


case class ScalaFieldInfo(
  index:                    Int,
  name:                     String,
  fieldType:                RType,
  annotations:              Map[String,Map[String,String]],
  defaultValueAccessorName: Option[(String,String)], // (class, method)  //Option[()=>Object],
  originalSymbol:           Option[TypeSymbol],
  isNonConstructorField:    Boolean = false
) extends FieldInfo:

  def valueOf[T](target: T) = target.getClass.getMethod(name).invoke(target)
  def constructorClass: Class[_] = constructorClassFor(fieldType)

  def reIndex(i: Int): FieldInfo = this.copy(index = i)

  lazy val defaultValue: Option[Object] = defaultValueAccessorName.map{ (companionClass, accessor) =>
    val companion = Class.forName(companionClass)
    val cons = companion.getDeclaredConstructors()
    cons(0).setAccessible(true)
    val companionInst = cons(0).newInstance()
    companion.getMethod(accessor).invoke(companionInst)
  }

  private def constructorClassFor(t: RType): Class[_] = 
    t match {
      case _: TypeSymbolInfo => classOf[Object] // Magic: Java constructors set param type to Object if it is a parameterized type
      case info: UnionInfo => classOf[Object]  // Union-typed constructors translate to Object in Java, so...
      case _ if originalSymbol.isDefined => classOf[Object]
      case info: AliasInfo => info.unwrappedType.infoClass
      case info: ScalaCaseClassInfo if info.isValueClass => info.fields.head.fieldType.infoClass
      case info: ScalaEnumerationInfo => Class.forName("scala.Enumeration$Value")
      case info => info.infoClass
    }

/* This is also used for plain-class getter/setter fields */
case class JavaFieldInfo(
  index:           Int,
  name:            String,
  fieldType:       RType,
  annotations:     Map[String,Map[String,String]],
  valueAccessor:   Method,
  valueSetter:     Method,
  originalSymbol:  Option[TypeSymbol]
) extends FieldInfo:
  lazy val defaultValue = None
  def valueOf[T](target: T): Object = valueAccessor.invoke(target)
  def setValue[T](target: T, theValue: Object) = valueSetter.invoke(target, theValue)
  def reIndex(i: Int): FieldInfo = this.copy(index = i)

