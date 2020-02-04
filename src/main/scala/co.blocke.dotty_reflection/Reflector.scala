package co.blocke.dotty_reflection

import scala.tasty.inspector._
import impl._
import scala.reflect.ClassTag

object Reflector

  private val cache = scala.collection.mutable.HashMap.empty[String, ReflectedThing]

  def reflectOn[T](given ct: ClassTag[T]): ReflectedThing = 
    val clazz = ct.runtimeClass
    reflectOn(clazz)

  def reflectOn[T](clazz: Class[T]): ReflectedThing =
    val className = clazz.getName
    cache.getOrElse(className,{
      val tc = new TastyClassInspector[T](clazz, cache)
      tc.inspect("", List(className))
      cache(className)
    })
