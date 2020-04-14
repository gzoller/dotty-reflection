package co.blocke.dotty_reflection
package info

case class ObjectInfo protected[dotty_reflection](
    name: String,
    infoClass: Class[_]
  ) extends ConcreteType:

  val orderedTypeParameters: List[TypeSymbol] = Nil

  def resolveTypeParams(actualTypeMap: Map[TypeSymbol, RType]): ConcreteType = this /* TODO */

  def show(tab: Int = 0, supressIndent: Boolean = false): String = 
    {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName + s"($name)\n"
