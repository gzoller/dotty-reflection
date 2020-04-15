package co.blocke.dotty_reflection
package impl


trait ClassOrTrait:
  self: RType =>

  // Run up the interitance tree to the top (Object) to get all the superclasses and mixin interfaces of this one
  private def getSuperclasses(c: Class[_] = infoClass, stack:List[String] = List.empty[String]): List[String] = 
    val ammendedStack = (stack :+ c.getName) ::: c.getInterfaces.toList.map(_.getName)
    val sc = c.getSuperclass()
    if( sc == classOf[Object] || sc == null)
      ammendedStack
    else 
      getSuperclasses(sc, ammendedStack)

  lazy val superclassEcosystem = getSuperclasses()

  // Does this class either implement the given mixin name, or inherit from a class named for the mixin?
  def hasMixin(mixin: String): Boolean = superclassEcosystem.contains(mixin)