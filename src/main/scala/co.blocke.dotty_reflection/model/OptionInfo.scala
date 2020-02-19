package co.blocke.dotty_reflection
package model

import java.lang.reflect._

trait OptionInfo extends ReflectedThing {
  val optionParamType: ALL_TYPE
}

case class ScalaOptionInfo(
  name: String,
  optionParamType: ALL_TYPE
) extends OptionInfo {
  val typeParameters = Nil

  def isA2( arg: Object ): Boolean =
    println("arg is : "+Reflector.reflectOnClass(arg.asInstanceOf[Some[_]].get.getClass))
    println("Option is : "+optionParamType)
    arg == None || optionParamType == Reflector.reflectOnClass(arg.asInstanceOf[Some[_]].get.getClass)    

  def isA(c: Class[_]): Boolean = 
    c == None.getClass || (c.getName == "scala.Some" && isOk(c))
  
  private def isOk(c: Class[_]) =
    println("-- 1: "+c.getTypeParameters.toList)
    println("-- 2: "+c.getGenericSuperclass)
    println("-- 3: "+c.getTypeParameters.head.getAnnotatedBounds.toList)
    println("-- 4: "+c.getTypeParameters.head.getBounds.toList)
    println("-- 5: "+c.getTypeParameters.head.getGenericDeclaration)
    println("-- 6: "+c.getTypeParameters.head.getName)
    false
    /*
    println(">> "+c.getName)
    val paramMatches = optionParamType match {
      case r: ReflectedThing => r.name == c.getTypeParameters.head.getName
      case p: PrimitiveType => 
        println("HERE: "+c.getTypeParameters.head)
        println("THERE: "+c.getGenericSuperclass.asInstanceOf[ParameterizedType].getOwnerType)
        println("FOOM: "+c.getGenericSuperclass.asInstanceOf[ParameterizedType].getRawType)
        false
      case _:TypeSymbol => true
    }
    c.getName == name && paramMatches
    */
}

case class JavaOptionInfo(
  name: String,
  optionParamType: ALL_TYPE
) extends OptionInfo {
  val typeParameters = Nil
  def isA(c: Class[_]): Boolean = false // TODO
}