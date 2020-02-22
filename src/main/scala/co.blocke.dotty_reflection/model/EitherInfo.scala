package co.blocke.dotty_reflection
package model

import scala.util.{Left,Right}

case class ScalaEitherInfo(
  name: String,
  leftParamType: ALL_TYPE,
  rightParamType: ALL_TYPE
) extends ReflectedThing: //with UnionContainer

  val typeParameters = Nil

  // private def checkForUnion(t: ALL_TYPE) =
  //   t match {
  //     case f @ UnionKind() => true
  //     case _ => false
  //   }
  val hasUnion: Boolean = false //checkForUnion(leftParamType) || checkForUnion(rightParamType)

  def isA(c: Class[_]): Boolean = false // Not currently used

  def isA2( arg: Object ): Boolean =
    (arg.isInstanceOf[Left[_,_]] && {
      val leftArg = arg.asInstanceOf[Left[_,_]]
      leftParamType match {
        case _:TypeSymbol => throw new Exception("Unexpected type symbol")
        case ct:ReflectedThing => ct.isA(leftArg.value.getClass)
        case ct:PrimitiveType => ct.isA(leftArg.value.getClass)
        }
    }) || (arg.isInstanceOf[Right[_,_]] && {
      val rightArg = arg.asInstanceOf[Right[_,_]]
      rightParamType match {
        case _:TypeSymbol => throw new Exception("Unexpected type symbol")
        case ct:ReflectedThing => ct.isA(rightArg.value.getClass)
        case ct:PrimitiveType => ct.isA(rightArg.value.getClass)
        }
    })