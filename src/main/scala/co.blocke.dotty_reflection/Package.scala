package co.blocke.dotty_reflection


/** Mnemonic symbol for a type--typically a paramaterized type, e.g. Foo[T], where T is the symbol */
opaque type TypeSymbol = String 

/** A union type is resolved to AnyRef, which isn't helpful.  This is a marker class name to differentiate a union type */
val UNION_CLASS = "__union_type__"

/** An intersection type is resolved to AnyRef, which isn't helpful.  This is a marker class name to differentiate a union type */
val INTERSECTION_CLASS = "__intersection_type__"

/** Java Arrays devolve into java.util.List, which isn't quite the same thing, so we created this placeholder */
val JAVA_ARRAY_CLASS = "__array__"

/** Any is an abstract class in Scala, so Class.forName() won't work.  Need this marker. */
val ANY_CLASS = "scala.Any"

class ReflectException(msg: String) extends Exception(msg)

val ENUM_CLASSNAME = "scala.Enumeration.Value"