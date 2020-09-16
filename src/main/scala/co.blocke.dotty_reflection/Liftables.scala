package co.blocke.dotty_reflection

import quoted._
import java.io._
import java.util._
import Liftable._

import info._

given Liftable[TypeSymbol] {
  def toExpr(t: TypeSymbol) = '{ ${Expr(t.asInstanceOf[String])}.asInstanceOf[TypeSymbol] }
}

given Liftable[Transporter.RType] {
  def toExpr(x: Transporter.RType) =
    '{ RType.deserialize(${Expr(x.serialize) }).asInstanceOf[Transporter.RType] }
}

given Liftable[TypeMemberInfo] {
  def toExpr(x: TypeMemberInfo) =
    '{ new TypeMemberInfo(${Expr(x.name)}, ${Expr(x.typeSymbol)}, ${ Expr(x.memberType) } ) }
}

given Liftable[SelfRefRType] {
  def toExpr(x: SelfRefRType) =
    '{ new SelfRefRType(${Expr(x.name)}) }
}
