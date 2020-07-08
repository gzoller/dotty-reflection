package co.blocke.dotty_reflection
import impl._
// import scala.runtime.Statics.releaseFence

/** Mnemonic symbol for a type--typically a paramaterized type, e.g. Foo[T], where T is the symbol */
opaque type TypeSymbol = String 

/** A union type is resolved to AnyRef, which isn't helpful.  This is a marker class name to differentiate a union type */
val UNION_CLASS = "__union_type__"

/** An intersection type is resolved to AnyRef, which isn't helpful.  This is a marker class name to differentiate a union type */
val INTERSECTION_CLASS = "__intersection_type__"

class ReflectException(msg: String) extends Exception(msg)

val ENUM_CLASSNAME = "scala.Enumeration.Value"

val typesymregx = """.*\.\_\$(.+)$""".r


/*
// Need this cache because apparently calling paramSymss mutates states and crashes on repeated calls!
import dotty.tools.dotc.core.Symbols.{Symbol => CoreSymbol}
private val mm = new java.util.concurrent.ConcurrentHashMap[CoreSymbol, List[TypeSymbol]]

def getTypeParameters(reflect: scala.tasty.Reflection)(symbol: reflect.Symbol): List[TypeSymbol] = 
  // Arrays are "special" for some reason--other collections work here but Arrays blow up!
  symbol match {
    case sym if sym.fullName == Clazzes.ScalaArrayClazz.getName => List("T".asInstanceOf[TypeSymbol])
    case _ =>
      this.synchronized {
        Option(mm.get(symbol.asInstanceOf[CoreSymbol])).getOrElse{
          val syms = symbol.primaryConstructor.paramSymss match {
            case Nil => Nil
            case p if p.nonEmpty  => p.head.filter(_.isType).map(_.name.asInstanceOf[TypeSymbol])
            case _   => Nil
          }
          mm.put(symbol.asInstanceOf[CoreSymbol],syms)
          syms
        }
      }
    }
    */

def mangleArrayClassName(tpe: RType): String =
  val mangled = tpe match {
    case _: info.TypeSymbolInfo => "Ljava.lang.Object;"
    case c: info.ArrayInfo => mangleArrayClassName(c.elementType)
    case c: info.JavaArrayInfo => mangleArrayClassName(c.elementType)
    case PrimitiveType.Scala_Boolean => "Z"
    case PrimitiveType.Scala_Byte => "B"
    case PrimitiveType.Scala_Char => "C"
    case PrimitiveType.Scala_Double => "D"
    case PrimitiveType.Scala_Float => "F"
    case PrimitiveType.Scala_Int => "I"
    case PrimitiveType.Scala_Long => "J"
    case PrimitiveType.Scala_Short => "S"
    case PrimitiveType.Scala_Any => "Ljava.lang.Object;"
    case c => "L" + c.name + ";"
  }
  "[" + mangled


extension ListOps on [A,B](xs: List[A]) {
  def findMap( p: (A) => Option[B] ): Option[B] = 
    var these: List[A] = xs
    while (!these.isEmpty) {
      val pRet = p(these.head)
      if pRet.isDefined then return pRet
      these = these.tail
    }
    None

  def filterMap(p: A => Option[B]): List[B] = 
    def doit(l: List[A], acc: List[B]): List[B] = {
      if (l.isEmpty)
        acc
      else {
        val retVal = p(l.head)
        val newAcc = if retVal.isDefined then
            acc :+ retVal.get
          else 
            acc
        doit(l.tail, newAcc)
      }
    }
    val result = doit(xs, Nil)
    // releaseFence()  <--- Not sure why this is needed, or what it does!
    result
}
