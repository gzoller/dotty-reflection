package co.blocke.dotty_reflection
package model


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
