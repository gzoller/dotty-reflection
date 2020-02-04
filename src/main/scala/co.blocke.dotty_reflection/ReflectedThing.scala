package co.blocke.dotty_reflection

type ALL_TYPE = ReflectedThing | PrimitiveType | TypeSymbol
  
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
  val unionTypes: List[ALL_TYPE]
  ) extends ReflectedThing

case class StaticAliasInfo protected (
  definedType: String,
  unwrappedType: ALL_TYPE
  ) extends ReflectedThing with IsAable 
    val name: String = definedType.drop(definedType.lastIndexOf('.')+1)
    val isUnion = unwrappedType.isInstanceOf[StaticUnionInfo]
    def isA(c: Class[_]): Boolean = unwrappedType match {
      case i: IsAable => 
        println("Type "+definedType+ "+ isa "+c.getName+" : "+i.isA(c))
        i.isA(c)
      case _ => 
        println("Type "+definedType+ "+ isa "+c.getName+" : false")
        false
    }
    val typeParameters = List.empty[TypeSymbol] // unused for aliases


object UnionKind
  def unapply(f: FieldInfo): Boolean = 
    f.fieldType match {
      case _: StaticUnionInfo => true
      case t: StaticAliasInfo if t.isUnion =>  true
      case _ => false
    }


case class StaticClassInfo protected (
  val name: String,
  val fields: List[FieldInfo],
  val typeParameters: List[TypeSymbol],
  val annotations: Map[String, Map[String,String]],
  val isValueClass: Boolean
  ) extends ReflectedThing with ClassOrTrait with IsAable {

    // Classes need to be matched on: 1) exact class match, 2) implementing interfaces, 3) parent class inheritance
    def isA(c: Class[_]): Boolean = superclassEcosystem.contains(c.getName)

    private val constructor = clazz.getConstructor(fields.map(_.constructorClass):_*)
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
        case ci: StaticClassInfo if ci.isA(arg.getClass) => idx
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


case class StaticTraitInfo protected(name: String, typeParameters: List[TypeSymbol]) extends ReflectedThing with ClassOrTrait with IsAable 
  // Traits need to match the whole ecosystem graph of the arg and that of the type
  def isA(c: Class[_]): Boolean = superclassEcosystem.intersect(getSuperclasses(c)).nonEmpty

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