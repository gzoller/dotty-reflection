package co.blocke.dotty_reflection
package infos

case class TupleInfo protected[dotty_reflection](
  name: String,
  infoClass: Class[_],
  tupleTypes: List[ALL_TYPE],
  typeParamSymbols: List[Option[TypeSymbol]]
) extends ConcreteType:

  val typeParameters = infoClass.getTypeParameters.toList.map(_.getName.asInstanceOf[TypeSymbol])

  override def sewTypeParams(actualTypeMap: Map[TypeSymbol, ALL_TYPE]): ConcreteType = 
    val together = tupleTypes.zip(typeParamSymbols)
    val resolvedTupleTypes: List[ALL_TYPE] = together.map{ (tType, tSym) => tSym match {
      case Some(s) if actualTypeMap.contains(s) => actualTypeMap(s)
      case None if tType.isInstanceOf[ConcreteType] => tType.asInstanceOf[ConcreteType].sewTypeParams(actualTypeMap)
      case None => tType
      }
    }
    this.copy(tupleTypes = resolvedTupleTypes)
    /*
    this.copy(tupleTypes = tupleTypes.map(_ match {
      case ts: TypeSymbol if actualTypeMap.contains(ts) => 
        println("--1--: "+ts)
        actualTypeMap(ts)
      case ts: TypeSymbol => 
        println("--2--")
        this
      case c: ConcreteType => 
        println("--3-- "+c)
        c.sewTypeParams(actualTypeMap)
  }))

  typeParamSymbol match {
    case Some(ts) if actualTypeMap.contains(ts) =>  // 1st level direct type substitution
      this.copy(fieldType = actualTypeMap(ts) )
    case ct: ConcreteType => // nth level -- may be a substitution--or not
      this.copy(fieldType = ct.sewTypeParams(actualTypeMap))
    case _ => this
  }
  */
