package co.blocke.dotty_reflection
package model


trait ClassInfo extends ReflectedThing with ClassOrTrait with IsAable
  val name: String
  val fields: List[FieldInfo]
  val typeParameters: List[TypeSymbol]
  val annotations: Map[String, Map[String,String]]
  def constructWith[T](args: List[Object]): T 

  // Classes need to be matched on: 1) exact class match, 2) implementing interfaces, 3) parent class inheritance
  def isA(c: Class[_]): Boolean = superclassEcosystem.contains(c.getName)


case class StaticClassInfo protected (
  val name: String,
  val fields: List[FieldInfo],
  val typeParameters: List[TypeSymbol],
  val annotations: Map[String, Map[String,String]],
  val isValueClass: Boolean
  ) extends ClassInfo {

    private val constructor = clazz.getConstructor(fields.map(_.asInstanceOf[ScalaFieldInfo].constructorClass):_*)
    private val unionTypedFields = 
      fields.collect {
        case f @ UnionKind() => f.index
      }
    lazy val mixins: List[String] = clazz.getInterfaces.toList.map(_.getName)

    // All this nonsense is because Scala 3 union typed fields are thrown away to java.lang.Object in the constructor.
    // That means we have to manually check the type of the given argument against any of the expected/allowed types
    // in the union.  Otherwise the JVM machinery is perfectly happy to construct a class with parameters that don't
    // match anything in the union--completely ignoring the type system!!!
    private def unionArgOk( arg: Object, idx: Int ) = 
      val unionKind = fields(idx).fieldType match {
        case s: StaticUnionInfo => s
        case a: StaticAliasInfo => a.unwrappedType.asInstanceOf[StaticUnionInfo] // safe cast if we got to this point!
        case _ => throw new Exception("Boom")  // Should Never Happen(tm)
      }
      unionKind.unionTypes.collectFirst{
        case ci: ClassInfo if ci.isA(arg.getClass) => idx
        case ai: StaticAliasInfo if ai.isA(arg.getClass) => idx
        case ti: StaticTraitInfo if ti.isA(arg.getClass) => idx
        case p: PrimitiveType if(p.isA(arg.getClass)) => idx
        case _:TypeSymbol => idx // Sure... anything goes for 'T'.... why not?
        // TODO: Check for alias of union type:  opaque type foo = bar | other
      }

    def constructWith[T](args: List[Object]): T = 
      unionTypedFields.map( utf => unionArgOk(args(utf), utf ).orElse(throw new java.lang.IllegalArgumentException("argument type mismatch")))
      constructor.newInstance(args:_*).asInstanceOf[T]
  }

  
case class StaticJavaClassInfo protected (
  val name: String,
  val fields: List[FieldInfo],
  val typeParameters: List[TypeSymbol],
  val annotations: Map[String, Map[String,String]],
  ) extends ClassInfo {

    def constructWith[T](args: List[Object]): T = null.asInstanceOf[T]

  }
