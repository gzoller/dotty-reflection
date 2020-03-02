package co.blocke.dotty_reflection
package model

/** Arity 1 Collections, e.g. List, Set, Seq */
case class Collection_A1_Info(
  name: String,
  typeParameters: List[TypeSymbol],
  elementType: ALL_TYPE
) extends ConcreteType


/** Arity 2 Collections, Map flavors, basiclly */
case class Collection_A2_Info(
  name: String,
  typeParameters: List[TypeSymbol],
  elementType1: ALL_TYPE,
  elementType2: ALL_TYPE
) extends ConcreteType