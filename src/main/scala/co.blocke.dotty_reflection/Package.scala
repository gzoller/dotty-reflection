package co.blocke.dotty_reflection
import impl._
// import scala.runtime.Statics.releaseFence

/** Mnemonic symbol for a type--typically a paramaterized type, e.g. Foo[T], where T is the symbol */
opaque type TypeSymbol = String 

/** A union type is resolved to AnyRef, which isn't helpful.  This is a marker class name to differentiate a union type */
val UNION_CLASS = "Union"

/** An intersection type is resolved to AnyRef, which isn't helpful.  This is a marker class name to differentiate a union type */
val INTERSECTION_CLASS = "Intersection"

class ReflectException(msg: String) extends Exception(msg)

val ENUM_CLASSNAME = "scala.Enumeration.Value"

val typesymregx = """.*\.\_\$(.+)$""".r

    
def mangleArrayClassName(tpe: Transporter.RType): String =
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
