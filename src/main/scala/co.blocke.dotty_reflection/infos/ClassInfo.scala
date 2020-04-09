package co.blocke.dotty_reflection
package infos


trait ClassInfo extends ConcreteType with ClassOrTrait:
  val name: String
  val fields: List[FieldInfo]
  val typeParameters: List[TypeSymbol]
  val annotations: Map[String, Map[String,String]]
  def constructWith[T](args: List[Object]): T 


case class TypeMember(
    name:                String,     // name of the type
    typeSymbol:          TypeSymbol, // signature (i.e. the generic letter 'T', e.g. Foo[T]
    baseType:            ALL_TYPE    // defined type (likely a trait)

    // This is only used for PlainClass writes -- typeValueModifier processing --> can be set at runtime only in ScalaJack
    // runtimeConcreteType: Option[ConcreteType] = None // inferred concrete type reflecting on actual class (or materialized from input)
)


case class ScalaClassInfo protected[dotty_reflection] (
  name: String,
  infoClass: Class[_],
  typeMembers: List[TypeMember],
  fields: List[FieldInfo],
  typeParameters: List[TypeSymbol],
  annotations: Map[String, Map[String,String]],
  isValueClass: Boolean
  ) extends ClassInfo:

    private lazy val constructor = infoClass.getConstructor(fields.map(_.asInstanceOf[ScalaFieldInfo].constructorClass):_*)

    def constructWith[T](args: List[Object]): T = constructor.newInstance(args:_*).asInstanceOf[T]

    override def sewTypeParams(actualTypeMap: Map[TypeSymbol, ALL_TYPE]): ConcreteType = 
      this match {
        case ci if ci.typeParameters != Nil =>
          val fixedFields = fields.map( _.sewTypeParams( actualTypeMap ))
          this.copy(fields = fixedFields)
        case ci => ci
      } match {
        case ci if ci.typeMembers != Nil =>
          val newTypeMembers = ci.typeMembers.map(tm => tm.copy(baseType = actualTypeMap(tm.typeSymbol)))
          println("Deeper: "+newTypeMembers)
          ci.copy(typeMembers = newTypeMembers)
        case ci => ci
      }

  
case class JavaClassInfo protected[dotty_reflection] (
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

    override def sewTypeParams(actualTypeMap: Map[TypeSymbol, ALL_TYPE]) = 
      if typeParameters != Nil then // Only sew down if this class is parameterized, otherwise it's already fully resolved.
        val fixedFields = fields.map( _.sewTypeParams( actualTypeMap ))
        this.copy(fields = fixedFields)
      else 
        this
