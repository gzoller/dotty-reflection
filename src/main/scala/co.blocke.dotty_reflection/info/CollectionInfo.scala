package co.blocke.dotty_reflection
package info

/** Arity 1 Collections, e.g. List, Set, Seq */
case class SeqLikeInfo protected[dotty_reflection](
  name: String,
  infoClass: Class[_],
  elementType: RType
) extends ConcreteType with CollectionType:
  val orderedTypeParameters = infoClass.getTypeParameters.toList.map(_.getName.asInstanceOf[TypeSymbol])



  /*
  override def sewTypeParams(actualTypeMap: Map[TypeSymbol, RType]): ConcreteType = 
    elementType match {
      case ts: TypeSymbol if actualTypeMap.contains(ts) => this.copy(elementType = actualTypeMap(ts))
      case ts: TypeSymbol => this
      case c: ConcreteType => this.copy(elementType = c.sewTypeParams(actualTypeMap))
    }
    */

/** Arity 2 Collections, Map flavors, basiclly */
case class MapLikeInfo protected[dotty_reflection](
  name: String,
  infoClass: Class[_],
  elementType: RType,
  elementType2: RType
) extends ConcreteType with CollectionType:
  val orderedTypeParameters = infoClass.getTypeParameters.toList.map(_.getName.asInstanceOf[TypeSymbol])

  override def show(tab: Int = 0, supressIndent: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName 
    + s"($name)" + {if orderedTypeParameters.nonEmpty then s"""[${orderedTypeParameters.mkString(",")}]:\n""" else ":\n"}
    + elementType.show(newTab)
    + elementType2.show(newTab)

  /*
  override def sewTypeParams(actualTypeMap: Map[TypeSymbol, RType]): ConcreteType = 
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
    */

/** Scala Array */
case class ArrayInfo protected[dotty_reflection](
  name: String,
  infoClass: Class[_],
  elementType: RType
) extends ConcreteType:
  val orderedTypeParameters = infoClass.getTypeParameters.toList.map(_.getName.asInstanceOf[TypeSymbol])

  def show(tab: Int = 0, supressIndent: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + s"array of " + elementType.show(newTab,true)

  /*
  override def sewTypeParams(actualTypeMap: Map[TypeSymbol, RType]): ConcreteType = 
    typeParameters match {
      case ts :: _ if actualTypeMap.contains(ts) => 
        this.copy(elementType = actualTypeMap(ts))
      case Nil => 
        this.copy(elementType = elementType.asInstanceOf[ConcreteType].sewTypeParams(actualTypeMap))
      case _ => 
        this
    }
    */

/** Java Set dirivative */
case class JavaSetInfo protected[dotty_reflection](
  name: String,
  infoClass: Class[_],
  orderedTypeParameters: List[TypeSymbol],
  elementType: RType
) extends ConcreteType with CollectionType
/*
  override def sewTypeParams(actualTypeMap: Map[TypeSymbol, RType]): ConcreteType = 
    elementType match {
      case ts: TypeSymbol if actualTypeMap.contains(ts) => this.copy(elementType = actualTypeMap(ts))
      case ts: TypeSymbol => this
      case c: ConcreteType => this.copy(elementType = c.sewTypeParams(actualTypeMap))
    }
    */

/** Java List dirivative */
case class JavaListInfo protected[dotty_reflection](
  name: String,
  infoClass: Class[_],
  orderedTypeParameters: List[TypeSymbol],
  elementType: RType
) extends ConcreteType with CollectionType
/*
  override def sewTypeParams(actualTypeMap: Map[TypeSymbol, RType]): ConcreteType = 
    elementType match {
      case ts: TypeSymbol if actualTypeMap.contains(ts) => this.copy(elementType = actualTypeMap(ts))
      case ts: TypeSymbol => this
      case c: ConcreteType => this.copy(elementType = c.sewTypeParams(actualTypeMap))
    }
    */

/** Java Array */
case class JavaArrayInfo protected[dotty_reflection](
  infoClass: Class[_],
  elementType: RType
) extends ConcreteType:
  val name: String = Reflector.JAVA_ARRAY_CLASS
  val orderedTypeParameters: List[TypeSymbol] = Nil

  def show(tab: Int = 0, supressIndent: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + s"array of " + elementType.show(newTab,true)

  /*
  override def sewTypeParams(actualTypeMap: Map[TypeSymbol, RType]): ConcreteType = 
    elementType match {
      case ts: TypeSymbol if actualTypeMap.contains(ts) => this.copy(elementType = actualTypeMap(ts))
      case ts: TypeSymbol => this
      case c: ConcreteType => this.copy(elementType = c.sewTypeParams(actualTypeMap))
    }
    */

/** Java Queue dirivative */
case class JavaQueueInfo protected[dotty_reflection](
  name: String,
  infoClass: Class[_],
  orderedTypeParameters: List[TypeSymbol],
  elementType: RType
) extends ConcreteType with CollectionType
/*
  override def sewTypeParams(actualTypeMap: Map[TypeSymbol, RType]): ConcreteType = 
    elementType match {
      case ts: TypeSymbol if actualTypeMap.contains(ts) => this.copy(elementType = actualTypeMap(ts))
      case ts: TypeSymbol => this
      case c: ConcreteType => this.copy(elementType = c.sewTypeParams(actualTypeMap))
    }
    */

/** Java Stack dirivative */
case class JavaStackInfo protected[dotty_reflection](
  name: String,
  infoClass: Class[_],
  orderedTypeParameters: List[TypeSymbol],
  elementType: RType
) extends ConcreteType with CollectionType
/*
  override def sewTypeParams(actualTypeMap: Map[TypeSymbol, RType]): ConcreteType = 
    elementType match {
      case ts: TypeSymbol if actualTypeMap.contains(ts) => this.copy(elementType = actualTypeMap(ts))
      case ts: TypeSymbol => this
      case c: ConcreteType => this.copy(elementType = c.sewTypeParams(actualTypeMap))
    }
    */

/** Java Set dirivative */
case class JavaMapInfo protected[dotty_reflection](
  name: String,
  infoClass: Class[_],
  orderedTypeParameters: List[TypeSymbol],
  elementType: RType,
  elementType2: RType
) extends ConcreteType with CollectionType:

  override def show(tab: Int = 0, supressIndent: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName 
    + s"($name)" + {if orderedTypeParameters.nonEmpty then s"""[${orderedTypeParameters.mkString(",")}]:\n""" else ":\n"}
    + elementType.show(newTab)
    + elementType2.show(newTab)
/*
  override def sewTypeParams(actualTypeMap: Map[TypeSymbol, RType]): ConcreteType = 
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
*/