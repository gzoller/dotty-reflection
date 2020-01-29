package co.blocke.dotty_reflection

trait ReflectedThing {
  val name: String
  val typeParameters: List[TypeSymbol]
}

case class StaticClassInfo protected (
  val name: String,
  val fields: List[FieldInfo],
  val typeParameters: List[TypeSymbol]
  ) extends ReflectedThing {
    private val clazz = Class.forName(name)
    private val constructor = {
      val fieldObjs = fields.map(_.constructorClass).toList
      clazz.getConstructor(fields.map(_.constructorClass):_*)
    }

    def constructWith[T](args: List[Object]): T = constructor.newInstance(args:_*).asInstanceOf[T]
  }

case class StaticTraitInfo protected(name: String, typeParameters: List[TypeSymbol]) extends ReflectedThing

// case class RuntimeClass() extends StaticClassInfo