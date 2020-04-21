package co.blocke.dotty_reflection

/** A materializable type */
trait RType:
  val name: String         /** typically the fully-qualified class name */
  val infoClass: Class[_]  /** the JVM class of this type */
  val orderedTypeParameters: List[TypeSymbol]  /** if this is a parameterized type,  list of type symbols in order of declaration */
  def show(tab: Int = 0, supressIndent: Boolean = false): String
  inline final def isParameterized: Boolean = !orderedTypeParameters.isEmpty
  override def toString(): String = show()


case class TypeMemberInfo(name: String, memberType: RType) extends RType {
  val orderedTypeParameters: List[TypeSymbol] = Nil
  val infoClass = Clazzes.ObjectClazz
  def show(tab: Int = 0, supressIndent: Boolean = false): String = 
    {if(!supressIndent) tabs(tab) else ""} + name + ": "+ memberType.show(tab+1, true)
}


case class TypeSymbolInfo(name: String) extends RType:
  val orderedTypeParameters: List[TypeSymbol] = Nil
  val infoClass = Clazzes.ObjectClazz
  def show(tab: Int = 0, supressIndent: Boolean = false): String = 
    {if(!supressIndent) tabs(tab) else ""} + name + "\n"


// Poked this here for now.  Used for show()
final inline def tabs(t:Int) = "   "*t
