package co.blocke.dotty_reflection
package impl

import info._
import scala.tasty.Reflection

/** Marker trait for all Scala/Java collections */
trait CollectionRType extends AppliedRType:
  self: RType =>

  lazy val elementType: RType

  def select(i: Int): RType = 
    if i == 0 then
      elementType
    else
      throw new ReflectException(s"AppliedType select index $i out of range for ${self.name}")

  def show(tab: Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName 
    + s"($name): "
    + elementType.show(newTab,name :: seenBefore,true)