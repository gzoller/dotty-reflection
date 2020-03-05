package co.blocke.dotty_reflection
package model

/** Arity 1 Collections, e.g. List, Set, Seq */
case class Collection_A1_Info(
  name: String,
  infoClass: Class[_],
  typeParameters: List[TypeSymbol],
  elementType: ALL_TYPE
) extends ConcreteType


/** Arity 2 Collections, Map flavors, basiclly */
case class Collection_A2_Info(
  name: String,
  infoClass: Class[_],
  typeParameters: List[TypeSymbol],
  elementType1: ALL_TYPE,
  elementType2: ALL_TYPE
) extends ConcreteType


/** Java Set dirivative */
case class JavaSetInfo(
  name: String,
  infoClass: Class[_],
  typeParameters: List[TypeSymbol],
  elementType: ALL_TYPE
) extends ConcreteType

/** Java List dirivative */
case class JavaListInfo(
  name: String,
  infoClass: Class[_],
  typeParameters: List[TypeSymbol],
  elementType: ALL_TYPE
) extends ConcreteType

/** Java Array */
case class JavaArrayInfo(
  elementType: ALL_TYPE
) extends ConcreteType {
  val name: String = "__array__"
  val typeParameters: List[TypeSymbol] = Nil
}

/** Java Queue dirivative */
case class JavaQueueInfo(
  name: String,
  infoClass: Class[_],
  typeParameters: List[TypeSymbol],
  elementType: ALL_TYPE
) extends ConcreteType

/** Java Set dirivative */
case class JavaMapInfo(
  name: String,
  infoClass: Class[_],
  typeParameters: List[TypeSymbol],
  elementType1: ALL_TYPE,
  elementType2: ALL_TYPE
) extends ConcreteType
