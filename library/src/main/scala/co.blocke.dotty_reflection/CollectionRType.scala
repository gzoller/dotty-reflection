package co.blocke.dotty_reflection


/** Marker trait for all Scala/Java collection *except* Arrays, which are a special case */
trait CollectionRType:
  self: RType =>

  lazy val elementType: RType

  def show(tab: Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName 
    + s"($name): "
    + elementType.show(newTab,name :: seenBefore,true)