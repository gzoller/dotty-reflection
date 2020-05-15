package co.blocke.dotty_reflection
package info

/** Arity 1 Collections, e.g. List, Set, Seq */
case class SeqLikeInfo protected[dotty_reflection](
  name: String,
  infoClass: Class[_],
  elementType: RType
) extends RType with CollectionType:
  val orderedTypeParameters = infoClass.getTypeParameters.toList.map(_.getName.asInstanceOf[TypeSymbol])


/** Arity 2 Collections, Map flavors, basiclly */
case class MapLikeInfo protected[dotty_reflection](
  name: String,
  infoClass: Class[_],
  elementType: RType,
  elementType2: RType
) extends RType with CollectionType:
  val orderedTypeParameters = infoClass.getTypeParameters.toList.map(_.getName.asInstanceOf[TypeSymbol])

  override def show(tab: Int = 0, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName 
    + s"($name" + {if orderedTypeParameters.nonEmpty then s"""[${orderedTypeParameters.mkString(",")}]):\n""" else "):\n"}
    + elementType.show(newTab)
    + elementType2.show(newTab)


/** Scala Array */
case class ArrayInfo protected[dotty_reflection](
  name: String,
  infoClass: Class[_],
  elementType: RType
) extends RType:
  val orderedTypeParameters = infoClass.getTypeParameters.toList.map(_.getName.asInstanceOf[TypeSymbol])

  def show(tab: Int = 0, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + s"array of " + elementType.show(newTab,true)


/** Java Set dirivative */
case class JavaSetInfo protected[dotty_reflection](
  name: String,
  infoClass: Class[_],
  orderedTypeParameters: List[TypeSymbol],
  elementType: RType
) extends RType with CollectionType


/** Java List dirivative */
case class JavaListInfo protected[dotty_reflection](
  name: String,
  infoClass: Class[_],
  orderedTypeParameters: List[TypeSymbol],
  elementType: RType
) extends RType with CollectionType


/** Java Array */
case class JavaArrayInfo protected[dotty_reflection](
  infoClass: Class[_],
  elementType: RType
) extends RType:
  val name: String = JAVA_ARRAY_CLASS
  val orderedTypeParameters: List[TypeSymbol] = Nil

  def show(tab: Int = 0, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + s"array of " + elementType.show(newTab,true)


/** Java Queue dirivative */
case class JavaQueueInfo protected[dotty_reflection](
  name: String,
  infoClass: Class[_],
  orderedTypeParameters: List[TypeSymbol],
  elementType: RType
) extends RType with CollectionType


/** Java Stack dirivative */
case class JavaStackInfo protected[dotty_reflection](
  name: String,
  infoClass: Class[_],
  orderedTypeParameters: List[TypeSymbol],
  elementType: RType
) extends RType with CollectionType


/** Java Map dirivative */
case class JavaMapInfo protected[dotty_reflection](
  name: String,
  infoClass: Class[_],
  orderedTypeParameters: List[TypeSymbol],
  elementType: RType,
  elementType2: RType
) extends RType with CollectionType:

  override def show(tab: Int = 0, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName 
    + s"($name" + {if orderedTypeParameters.nonEmpty then s"""[${orderedTypeParameters.mkString(",")}]):\n""" else "):\n"}
    + elementType.show(newTab)
    + elementType2.show(newTab)
