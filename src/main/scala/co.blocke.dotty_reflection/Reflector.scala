package co.blocke.dotty_reflection

import scala.tasty.file._
import impl._
import scala.reflect.ClassTag

object Reflector

  private val cache = scala.collection.mutable.HashMap.empty[String, ClassInfo]

  def reflectOn[T](given ct: ClassTag[T]): ClassInfo = 
    val clazz = ct.runtimeClass
    val className = clazz.getName
    val tc = new TastyClassConsumer[T](clazz, cache)
    ConsumeTasty("", List(className), tc)
    cache(className)