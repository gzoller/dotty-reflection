package co.blocke.dotty_reflection
package infos

/** Arity 1 Collections, e.g. List, Set, Seq */
case class SeqLikeInfo protected[dotty_reflection](
  name: String,
  infoClass: Class[_],
  elementType: ALL_TYPE
) extends ConcreteType:
  val typeParameters = infoClass.getTypeParameters.toList.map(_.getName.asInstanceOf[TypeSymbol])
  override def sewTypeParams(actualTypeMap: Map[TypeSymbol, ALL_TYPE]): ConcreteType = 
    elementType match {
      case ts: TypeSymbol if actualTypeMap.contains(ts) => this.copy(elementType = actualTypeMap(ts))
      case ts: TypeSymbol => this
      case c: ConcreteType => this.copy(elementType = c.sewTypeParams(actualTypeMap))
    }

/** Arity 2 Collections, Map flavors, basiclly */
case class MapLikeInfo protected[dotty_reflection](
  name: String,
  infoClass: Class[_],
  elementType1: ALL_TYPE,
  elementType2: ALL_TYPE
) extends ConcreteType:
  val typeParameters = infoClass.getTypeParameters.toList.map(_.getName.asInstanceOf[TypeSymbol])
  override def sewTypeParams(actualTypeMap: Map[TypeSymbol, ALL_TYPE]): ConcreteType = 
    val fixET1 = elementType1 match {
      case ts: TypeSymbol if actualTypeMap.contains(ts) => actualTypeMap(ts)
      case ts: TypeSymbol => this
      case c: ConcreteType => c.sewTypeParams(actualTypeMap)
    }
    val fixET2 = elementType2 match {
      case ts: TypeSymbol if actualTypeMap.contains(ts) => actualTypeMap(ts)
      case ts: TypeSymbol => this
      case c: ConcreteType => c.sewTypeParams(actualTypeMap)
    }
    this.copy(elementType1 = fixET1, elementType2 = fixET2)

/** Scala Array */
case class ArrayInfo protected[dotty_reflection](
  name: String,
  infoClass: Class[_],
  elementType: ALL_TYPE
) extends ConcreteType:
  val typeParameters = infoClass.getTypeParameters.toList.map(_.getName.asInstanceOf[TypeSymbol])
  override def sewTypeParams(actualTypeMap: Map[TypeSymbol, ALL_TYPE]): ConcreteType = 
    elementType match {
      case ts: TypeSymbol if actualTypeMap.contains(ts) => this.copy(elementType = actualTypeMap(ts))
      case ts: TypeSymbol => this
      case c: ConcreteType => this.copy(elementType = c.sewTypeParams(actualTypeMap))
    }

/** Java Set dirivative */
case class JavaSetInfo protected[dotty_reflection](
  name: String,
  infoClass: Class[_],
  typeParameters: List[TypeSymbol],
  elementType: ALL_TYPE
) extends ConcreteType:
  override def sewTypeParams(actualTypeMap: Map[TypeSymbol, ALL_TYPE]): ConcreteType = 
    elementType match {
      case ts: TypeSymbol if actualTypeMap.contains(ts) => this.copy(elementType = actualTypeMap(ts))
      case ts: TypeSymbol => this
      case c: ConcreteType => this.copy(elementType = c.sewTypeParams(actualTypeMap))
    }

/** Java List dirivative */
case class JavaListInfo protected[dotty_reflection](
  name: String,
  infoClass: Class[_],
  typeParameters: List[TypeSymbol],
  elementType: ALL_TYPE
) extends ConcreteType:
  override def sewTypeParams(actualTypeMap: Map[TypeSymbol, ALL_TYPE]): ConcreteType = 
    elementType match {
      case ts: TypeSymbol if actualTypeMap.contains(ts) => this.copy(elementType = actualTypeMap(ts))
      case ts: TypeSymbol => this
      case c: ConcreteType => this.copy(elementType = c.sewTypeParams(actualTypeMap))
    }

/** Java Array */
case class JavaArrayInfo protected[dotty_reflection](
  elementType: ALL_TYPE
) extends ConcreteType:
  val name: String = Reflector.JAVA_ARRAY_CLASS
  val typeParameters: List[TypeSymbol] = Nil
  override def sewTypeParams(actualTypeMap: Map[TypeSymbol, ALL_TYPE]): ConcreteType = 
    elementType match {
      case ts: TypeSymbol if actualTypeMap.contains(ts) => this.copy(elementType = actualTypeMap(ts))
      case ts: TypeSymbol => this
      case c: ConcreteType => this.copy(elementType = c.sewTypeParams(actualTypeMap))
    }

/** Java Queue dirivative */
case class JavaQueueInfo protected[dotty_reflection](
  name: String,
  infoClass: Class[_],
  typeParameters: List[TypeSymbol],
  elementType: ALL_TYPE
) extends ConcreteType:
  override def sewTypeParams(actualTypeMap: Map[TypeSymbol, ALL_TYPE]): ConcreteType = 
    elementType match {
      case ts: TypeSymbol if actualTypeMap.contains(ts) => this.copy(elementType = actualTypeMap(ts))
      case ts: TypeSymbol => this
      case c: ConcreteType => this.copy(elementType = c.sewTypeParams(actualTypeMap))
    }

/** Java Set dirivative */
case class JavaMapInfo protected[dotty_reflection](
  name: String,
  infoClass: Class[_],
  typeParameters: List[TypeSymbol],
  elementType1: ALL_TYPE,
  elementType2: ALL_TYPE
) extends ConcreteType:
  override def sewTypeParams(actualTypeMap: Map[TypeSymbol, ALL_TYPE]): ConcreteType = 
    val fixET1 = elementType1 match {
      case ts: TypeSymbol if actualTypeMap.contains(ts) => actualTypeMap(ts)
      case ts: TypeSymbol => this
      case c: ConcreteType => c.sewTypeParams(actualTypeMap)
    }
    val fixET2 = elementType2 match {
      case ts: TypeSymbol if actualTypeMap.contains(ts) => actualTypeMap(ts)
      case ts: TypeSymbol => this
      case c: ConcreteType => c.sewTypeParams(actualTypeMap)
    }
    this.copy(elementType1 = fixET1, elementType2 = fixET2)
