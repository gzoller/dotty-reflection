package co.blocke.dotty_reflection

trait ClassInfo {
  val name: String
  val fields: List[FieldInfo]
  def constructWith(args: List[Object]): Any // must be cast to appropriate type
}

case class InspectedClass protected (
  name: String,
  fields: List[FieldInfo],
  ) extends ClassInfo {
    private val clazz = Class.forName(name)
    private val constructor = {
      val fieldObjs = fields.map(_.constructorClass).toList
      clazz.getConstructor(fields.map(_.constructorClass):_*)
    }

    def constructWith(args: List[Object]): Any = constructor.newInstance(args:_*)
  }

// case class RuntimeClass() extends ClassInfo