package co.blocke.dotty_reflection

import java.lang.reflect.Method

opaque type TypeSymbol = String // Placeholder type, e.g. T as in Foo[T](x: T)

case class FieldInfo(
  name: String,
  fieldType: ClassInfo | TypeSymbol,
  defaultValueAccessor: Option[()=>Any]
)

// Note:  If we ever do a macro-based version of this, FieldInfo will need to be refactored into a trait.   defaultValueAccessor must be liftable,
// meaning we need to capture the name of the companion class and the method.  In the Liftable[FieldInfo] thingy we'll need to cook defaultValueAccessor
// like we do now in TastyClassConsumer