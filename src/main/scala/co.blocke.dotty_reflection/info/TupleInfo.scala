package co.blocke.dotty_reflection
package info

case class TupleInfo protected[dotty_reflection](
  name: String,
  infoClass: Class[_],
  tupleTypes: List[RType]
) extends ConcreteType:

  val orderedTypeParameters = infoClass.getTypeParameters.toList.map(_.getName.asInstanceOf[TypeSymbol])

  def show(tab: Int = 0, supressIndent: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + s"""(\n${tupleTypes.map(_.show(newTab)).mkString}""" + tabs(tab) + ")\n"

  /*
  override def sewTypeParams(actualTypeMap: Map[TypeSymbol, ALL_TYPE]): ConcreteType = 
    val together = tupleTypes.zip(typeParamSymbols)
    val resolvedTupleTypes: List[ALL_TYPE] = together.map{ (tType, tSym) => tSym match {
      case Some(s) if actualTypeMap.contains(s) => actualTypeMap(s)
      case None if tType.isInstanceOf[ConcreteType] => tType.asInstanceOf[ConcreteType].sewTypeParams(actualTypeMap)
      case None => tType
      }
    }
    this.copy(tupleTypes = resolvedTupleTypes)
*/