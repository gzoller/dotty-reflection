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
  def hasMixin(className: String): Boolean = superclassEcosystem.contains(className)
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