package co.blocke.dotty_reflection

trait ReflectedThing {
  val name: String
  val typeParameters: List[TypeSymbol]
}

trait ClassOrTrait {
  val name: String
  protected lazy val clazz = Class.forName(name)

  protected def getSuperclasses(c: Class[_], stack:List[String] = List.empty[String]): List[String] = 
    val ammendedStack = (stack :+ c.getName) ::: c.getInterfaces.toList.map(_.getName)
    val sc = c.getSuperclass()
    if( sc == classOf[Object] || sc == null)
      ammendedStack
    else 
      getSuperclasses(sc, ammendedStack)

  lazy val superclassEcosystem = getSuperclasses(clazz)
}

case class StaticUnionInfo protected (
  val name: String,
  val typeParameters: List[TypeSymbol],
  val unionTypes: List[ReflectedThing | PrimitiveType | TypeSymbol]
  ) extends ReflectedThing

case class StaticClassInfo protected (
  val name: String,
  val fields: List[FieldInfo],
  val typeParameters: List[TypeSymbol],
  val annotations: Map[String, Map[String,String]]
  ) extends ReflectedThing with ClassOrTrait {
    private val constructor = clazz.getConstructor(fields.map(_.constructorClass):_*)
    private val unionTypedFields = fields.collect{ case f if f.fieldType.isInstanceOf[StaticUnionInfo] => f.index }

    // All this nonsense is because Scala 3 union typed fields are thrown away to java.lang.Object in the constructor.
    // That means we have to manually check the type of the given argument against any of the expected/allowed types
    // in the union.  Otherwise the machinery is perfectly happy to construct a class with parameters that don't
    // match anything in the union--completely ignoring the type system!!!
    private def unionArgOk( arg: Object, idx: Int ) = 
      fields(idx).fieldType.asInstanceOf[StaticUnionInfo].unionTypes.collectFirst{
        // Classes need to be matched on: 1) exact class match, 2) implementing interfaces, 3) parent class inheritance
        case ci: StaticClassInfo if ci.superclassEcosystem.contains(arg.getClass.getName) => idx
        // Traits need to match the whole ecosystem graph of the arg and that of the type
        case ci: StaticTraitInfo if ci.superclassEcosystem.intersect(getSuperclasses(arg.getClass)).nonEmpty => idx
        case PrimitiveType.Scala_Boolean if arg.getClass.getName == "java.lang.Boolean" => idx
        case PrimitiveType.Scala_Byte if arg.getClass.getName == "java.lang.Byte" => idx
        case PrimitiveType.Scala_Char if arg.getClass.getName == "java.lang.Character" => idx
        case PrimitiveType.Scala_Double if arg.getClass.getName == "java.lang.Double" => idx
        case PrimitiveType.Scala_Float if arg.getClass.getName == "java.lang.Float" => idx
        case PrimitiveType.Scala_Int if arg.getClass.getName == "java.lang.Integer" => idx
        case PrimitiveType.Scala_Long if arg.getClass.getName == "java.lang.Long" => idx
        case PrimitiveType.Scala_Short if arg.getClass.getName == "java.lang.Short" => idx
        case PrimitiveType.Scala_String if arg.getClass.getName == "java.lang.String" => idx
        case _:TypeSymbol => idx // Sure... anything goes for 'T'.... why not?
      }

    def constructWith[T](args: List[Object]): T = 
      unionTypedFields.map( utf => unionArgOk(args(utf), utf ).orElse(throw new java.lang.IllegalArgumentException("argument type mismatch")))
      constructor.newInstance(args:_*).asInstanceOf[T]
  }

case class StaticTraitInfo protected(name: String, typeParameters: List[TypeSymbol]) extends ReflectedThing with ClassOrTrait

// case class RuntimeClass() extends StaticClassInfo


/*
interface X
interface Y
interface Z
class A implements X
class B extends A implements Y
class C extends B implements Z

val foo = X | Int
check against arg of type C
*/