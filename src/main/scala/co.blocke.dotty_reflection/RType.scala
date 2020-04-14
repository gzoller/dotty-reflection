package co.blocke.dotty_reflection


/** Mnemonic symbol for a type--typically a paramaterized type, e.g. Foo[T], where T is the symbol */
opaque type TypeSymbol = String 


/** A materializable type */
trait ConcreteType:
  val name: String         /** typically the fully-qualified class name */
  val infoClass: Class[_]  /** the JVM class of this type */
  val orderedTypeParameters: List[TypeSymbol]  /** if this is a parameterized type,  list of type symbols in order of declaration */

  def show(tab: Int = 0, supressIndent: Boolean = false): String
  def resolveTypeParams(actualTypeMap: Map[TypeSymbol, RType]): ConcreteType


/** Marker trait for all Scala/Java collection *except* Arrays, which are a special case */
trait CollectionType:
  self: ConcreteType =>

  val elementType: RType

  def show(tab: Int = 0, supressIndent: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName 
    + s"($name)" + {if orderedTypeParameters.nonEmpty then s"""[${orderedTypeParameters.mkString(",")}]: """ else ": "}
    + elementType.show(newTab,true)


object RType:
  def apply( typeSym: TypeSymbol ): RType = RType(PrimitiveType.Java_Object, Some(typeSym))
  def apply( typeSym: TypeSymbol, typeMemberName: String ): RType = RType(PrimitiveType.Java_Object, Some(typeSym), Some(typeMemberName))

/** Highest level reflected type */
case class RType(
    concreteType: ConcreteType,

    /** Set if this RType was originally a parameterized type.  Note that when a type is a parameterized value, the concreteType
    *  is set to Scala_Any.  If the parameter type symbol is later resolved via resolveTypeParams(), the concreteType may be reset
    *  to the actual resolved type, but typeParam remains the type symbol. */
    typeParam: Option[TypeSymbol] = None,

    typeMemberName: Option[String] = None  // used only with thie RType is a class type member (typeMemberName is the synthetic field name of the type)
  ):


  /** If a class or trait is defined Foo[T](a: T), a's original concreteType is Scala_Any and typeParam is Some("T").  If we know
   *  that T should be an Int, resolveTypeParams (recursively) resolves the T parameter to be Scala_Int, but leaves typeParam as
   *  Some("T").
   */
  def resolveTypeParams(actualTypeMap: Map[TypeSymbol, RType]): RType = 
    typeParam match {
      case Some(s) if actualTypeMap.contains(s) => actualTypeMap(s).copy(typeParam = Some(s))
      case None => this.copy(concreteType = concreteType.resolveTypeParams(actualTypeMap))
      case _ => this
    }

  def show(tab: Int = 0, supressIndent: Boolean = false): String =
    val newTab = {if supressIndent then tab else tab+1}
    {if !supressIndent then tabs(tab) else ""} 
    + {if typeParam.isDefined then s"[${typeParam.getOrElse("")}]" else ""}
    + concreteType.show(newTab, true)

  override def toString(): String = show()


/** This is for all the classes we don't inspect.  These may be "invalid" or just not reflectable.
  * Rather than toss our exception cookies, we just return UnknownInfo and let the caller decide
  * how serious this is.  In the case of ScalaJack, it may be completely fine, for example UUID.
  * We can make a ScalaJack TypeAdapter for UUID without needing to inspect the type.  For some
  * other application an UnknownInfo might be a serious problem.
  */
case class UnknownInfo(infoClass: Class[_]) extends ConcreteType:
  val name = infoClass.getName
  val orderedTypeParameters = Nil
  def resolveTypeParams(actualTypeMap: Map[TypeSymbol, RType]): ConcreteType = this
  def show(tab: Int = 0, supressIndent: Boolean = false): String = 
    {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName + s"($name)\n"

final def tabs(t:Int) = "   "*t