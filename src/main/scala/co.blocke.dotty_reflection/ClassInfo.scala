package co.blocke.dotty_reflection

trait ClassInfo {
  val name: String
  val fields: List[FieldInfo]
}

case class InspectedClass protected (
  name: String,
  fields: List[FieldInfo]
  ) extends ClassInfo

// case class RuntimeClass() extends ClassInfo