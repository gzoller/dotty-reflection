package co.blocke.dotty_reflection
package info

import java.lang.reflect.Method

trait FieldInfo:
  val index:                Int
  val name:                 String
  val fieldType:            RType
  val annotations:          Map[String,Map[String,String]]
  val valueAccessor:        Method
  val defaultValueAccessor: Option[()=>Object]
  def show(tab: Int = 0, supressIndent: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} 
      + s"($index) $name: " 
      + fieldType.show(newTab,true) 
      + { if annotations.nonEmpty then tabs(newTab) + "annotations: " + annotations.toString else "" }

//  def sewTypeParams(actualTypeMap: Map[TypeSymbol, ALL_TYPE]): FieldInfo

  /* 
  TODO:  Do we need usesSymbols as an easy-access if this fieldType consumes a type symbole?
  For example: foo: (Int, T, String)
  Field foo isn't a type symbol, and never was.  But it uses T.
  */

case class ScalaFieldInfo(
  index:                Int,
  name:                 String,
  fieldType:            RType,
  annotations:          Map[String,Map[String,String]],
  valueAccessor:        Method,
  defaultValueAccessor: Option[()=>Object]
) extends FieldInfo:
  def valueOf(target: Object) = valueAccessor.invoke(target)
  def constructorClass: Class[_] = constructorClassFor(fieldType)
  /*
  def sewTypeParams(actualTypeMap: Map[TypeSymbol, ALL_TYPE]) = 
    typeParamSymbol match {
      case Some(ts) if actualTypeMap.contains(ts) =>  // 1st level direct type substitution
        this.copy(fieldType = actualTypeMap(ts) )
      case None if fieldType.isInstanceOf[ConcreteType] => // nth level -- may be a substitution--or not
        this.copy(fieldType = fieldType.asInstanceOf[ConcreteType].sewTypeParams(actualTypeMap))
      case _ => 
        this
    }
    */

  private def constructorClassFor(t: RType): Class[_] = 
    if( t.typeParam.isDefined )
      classOf[Object] // Magic: Java constructors set param type to Object if it is a parameterized type
    else t.concreteType match { 
      /*
      case info: UnionInfo => classOf[Object]  // Union-typed constructors translate to Object in Java, so...
      case info: AliasInfo => info.unwrappedType.concreteType.infoClass
      case info: ScalaClassInfo if info.isValueClass => info.fields.head.fieldType.concreteType.infoClass
      case info: ScalaEnumeration => Class.forName("scala.Enumeration$Value")
      */
      case info => info.infoClass

    /*
      case ci:UnionInfo => classOf[Object]  // Union-typed constructors translate to Object in Java, so...
      case ot:AliasInfo => constructorClassFor(ot.unwrappedType)
      case ci:ScalaClassInfo if ci.isValueClass => constructorClassFor(ci.fields(0).fieldType)
      case PrimitiveType.Scala_Boolean => implicitly[reflect.ClassTag[Boolean]].runtimeClass
      case PrimitiveType.Scala_Byte => implicitly[reflect.ClassTag[Byte]].runtimeClass
      case PrimitiveType.Scala_Char => implicitly[reflect.ClassTag[Char]].runtimeClass
      case PrimitiveType.Scala_Double => implicitly[reflect.ClassTag[Double]].runtimeClass
      case PrimitiveType.Scala_Float => implicitly[reflect.ClassTag[Float]].runtimeClass
      case PrimitiveType.Scala_Int => implicitly[reflect.ClassTag[Int]].runtimeClass
      case PrimitiveType.Scala_Long => implicitly[reflect.ClassTag[Long]].runtimeClass
      case PrimitiveType.Scala_Short => implicitly[reflect.ClassTag[Short]].runtimeClass
      case PrimitiveType.Scala_String => classOf[String]
      case PrimitiveType.Java_Object => classOf[Object]
      case PrimitiveType.Scala_Any => classOf[Any]
      case e: ScalaEnumeration => Class.forName("scala.Enumeration$Value")
      case other => other.infoClass // catch-all for when we just don't know
      */
    }


/* This is also used for Scala plain-class getter/setter fields */
case class JavaFieldInfo(
  index:           Int,
  name:            String,
  fieldType:       RType,
  annotations:     Map[String,Map[String,String]],
  valueAccessor:   Method,
  valueSetter:     Method
) extends FieldInfo:
  val defaultValueAccessor = None
  /*
  def sewTypeParams(actualTypeMap: Map[TypeSymbol, ALL_TYPE]) = 
    typeParamSymbol match {
      case Some(ts) if actualTypeMap.contains(ts) =>  // 1st level direct type substitution
        this.copy(fieldType = actualTypeMap(ts) )
      case None if fieldType.isInstanceOf[ConcreteType] => // nth level -- may be a substitution--or not
        this.copy(fieldType = fieldType.asInstanceOf[ConcreteType].sewTypeParams(actualTypeMap))
      case _ => 
        this
    }
    */