package co.blocke.dotty_reflection
package model


trait ClassInfo extends ConcreteType with ClassOrTrait:
  val name: String
  val fields: List[FieldInfo]
  val typeParameters: List[TypeSymbol]
  val annotations: Map[String, Map[String,String]]
  def constructWith[T](args: List[Object]): T 


case class StaticClassInfo protected (
  name: String,
  infoClass: Class[_],
  fields: List[FieldInfo],
  typeParameters: List[TypeSymbol],
  annotations: Map[String, Map[String,String]],
  isValueClass: Boolean
  ) extends ClassInfo:
    private val constructor = clazz.getConstructor(fields.map(_.asInstanceOf[ScalaFieldInfo].constructorClass):_*)
    def constructWith[T](args: List[Object]): T = constructor.newInstance(args:_*).asInstanceOf[T]

  
case class StaticJavaClassInfo protected (
  name: String,
  infoClass: Class[_],
  fields: List[FieldInfo],
  typeParameters: List[TypeSymbol],
  annotations: Map[String, Map[String,String]],
  ) extends ClassInfo:
    private val fieldsByName = fields.map(f => (f.name, f.asInstanceOf[JavaFieldInfo])).toMap
    def field(name: String): Option[JavaFieldInfo] = fieldsByName.get(name)

    def constructWith[T](args: List[Object]): T = 
      val c = Class.forName(name).getConstructors.head.newInstance()
      fields.zipWithIndex.foreach((f,a) => f.asInstanceOf[JavaFieldInfo].valueSetter.invoke(c,args(a)))
      c.asInstanceOf[T]
