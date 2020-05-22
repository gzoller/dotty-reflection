package co.blocke.dotty_reflection


/** Marker trait for all Scala/Java collection *except* Arrays, which are a special case */
trait CollectionType:
  self: RType =>

  lazy val elementType: RType

  def show(tab: Int = 0, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName 
    + s"($name" + {if orderedTypeParameters.nonEmpty then s"""[${orderedTypeParameters.mkString(",")}]): """ else "): "}
    + elementType.show(newTab,true)