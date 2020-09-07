package co.blocke.dotty_reflection
package info


case class TypeMemberInfo(name: String, typeSymbol: TypeSymbol, memberType: Transporter.RType) extends Transporter.RType {
  val fullName = name
  lazy val infoClass = impl.Clazzes.ObjectClazz
  def show(tab: Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    {if(!supressIndent) tabs(tab) else ""} + name + s"[$typeSymbol]: "+ memberType.show(tab+1,name :: seenBefore, true)
}
