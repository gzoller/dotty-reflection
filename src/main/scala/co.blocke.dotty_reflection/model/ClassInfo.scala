package co.blocke.dotty_reflection
package model


trait ClassInfo extends ReflectedThing with ClassOrTrait
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
        case a: AliasInfo => a.unwrappedType.asInstanceOf[StaticUnionInfo] // safe cast if we got to this point!
        case _ => throw new Exception("Boom")  // Should Never Happen(tm)
      }
      // TODO: Is MyClass[Int] isA MyClass[Boolean]?  Likely not.... need ClassInfo.isA (and traits) to consider type params for isA!
      unionKind.unionTypes.collectFirst{
        case ci: ClassInfo if ci.isA(arg.getClass) => idx
        case ai: AliasInfo if ai.isA(arg.getClass) => idx
        case ti: StaticTraitInfo if ti.isA(arg.getClass) => idx
        case oi: ScalaOptionInfo if oi.isA2(arg) => idx
        case ei: ScalaEitherInfo if ei.isA(arg.getClass) => idx
        case p: PrimitiveType if(p.isA(arg.getClass)) => idx
        case _:TypeSymbol => idx // Sure... anything goes for 'T'.... why not?
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

    private val fieldsByName = fields.map(f => (f.name, f.asInstanceOf[JavaFieldInfo])).toMap
    def field(name: String): Option[JavaFieldInfo] = fieldsByName.get(name)

    def constructWith[T](args: List[Object]): T = 
      val c = Class.forName(name).getConstructors.head.newInstance()
      fields.zipWithIndex.foreach((f,a) => f.asInstanceOf[JavaFieldInfo].valueSetter.invoke(c,args(a)))
      c.asInstanceOf[T]
  }
