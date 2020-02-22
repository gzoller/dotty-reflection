package co.blocke.dotty_reflection
package model


case class StaticUnionInfo protected (
  val name: String,
  val typeParameters: List[TypeSymbol],
  val unionTypes: List[ALL_TYPE]
  ) extends ReflectedThing:
  def isA(c: Class[_]): Boolean = false  // TODO


/** Extractor to see if a FieldInfo is in fact either a Union itself, or an alias that resolves to a Union */
object UnionKind:
  def unapply(f: FieldInfo): Boolean = 
    f.fieldType match {
      case _: StaticUnionInfo => true
      case t: AliasInfo if t.isUnion =>  true
      case t: UnionContainer if t.hasUnion => true
      case _ => false
    }

  // def unapply(f: FieldInfo): Boolean = unapply(f.fieldType)

  // def unapply(f: ALL_TYPE): Boolean = 
  //   f match {
  //     case _: StaticUnionInfo => true
  //     case t: AliasInfo if t.isUnion =>  true
  //     case t: UnionContainer if t.hasUnion => true
  //     case _ => false
  //   }