package co.blocke.dotty_reflection

import quoted._
import java.io._
import java.util._
import Liftable._

import info._
import impl.SelfRefRType

given Liftable[TypeSymbol] {
  def toExpr(t: TypeSymbol) = '{ ${Expr(t.asInstanceOf[String])}.asInstanceOf[TypeSymbol] }
}

given Liftable[RType] {
  def toExpr(x: RType) =
    '{ RType.deserialize(${Expr(x.serialize) }).asInstanceOf[RType] }
}

given Liftable[TypeMemberInfo] {
  def toExpr(x: TypeMemberInfo) =
    '{ new TypeMemberInfo(${Expr(x.name)}, ${Expr(x.typeSymbol)}, ${ Expr(x.memberType) } ) }
}

given Liftable[SelfRefRType] {
  def toExpr(x: SelfRefRType) =
    '{ new SelfRefRType(${Expr(x.name)}) }
}

//--------------------------- EXPERIMENTAL
type ExpType = Function1[Class[_],RType]
given Liftable[ExpType] {
  def toExpr( x: ExpType ) =
    '{ QType.deserialize(${Expr(QType.serialize(x)) }) }
}

object QType {
  def deserialize(s: String): ExpType =
    val data = java.util.Base64.getDecoder().decode( s )
    val ois  = new ObjectInputStream( new ByteArrayInputStream( data ) )
    val o    = ois.readObject()
    ois.close()
    o.asInstanceOf[ExpType]

  def serialize(obj: ExpType): String =
    val baos = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream( baos )
    oos.writeObject( obj )
    oos.close()
    java.util.Base64.getEncoder().encodeToString(baos.toByteArray())

}
//--------------------------- /EXPERIMENTAL