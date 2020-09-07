package co.blocke.dotty_reflection
package info


/** RType for unassigned type symbol, e.g. Foo[T]
 */
 case class TypeSymbolInfo(name: String) extends Transporter.RType:
   val fullName = name
   lazy val infoClass = impl.Clazzes.ObjectClazz
   def show(tab: Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
     {if(!supressIndent) tabs(tab) else ""} + name + "\n"
