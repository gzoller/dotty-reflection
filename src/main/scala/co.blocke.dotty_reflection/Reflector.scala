package co.blocke.dotty_reflection

import scala.tasty.inspector._
import impl._
import scala.reflect.ClassTag
import model.ReflectedThing

object Reflector

  private val cache = scala.collection.mutable.HashMap.empty[String, ReflectedThing]

  def reflectOn[T](given ct: ClassTag[T]): ReflectedThing = 
    val clazz = ct.runtimeClass
    reflectOnClass(clazz)

  def reflectOnClass[T](clazz: Class[T]): ReflectedThing =
    val className = clazz.getName
    cache.getOrElse(className,{
      val tc = new TastyClassInspector[T](clazz, cache)
      tc.inspect("", List(className))
      cache.get(className).getOrElse(null)
    })
