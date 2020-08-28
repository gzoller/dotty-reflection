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
    '{ deserialize(${Expr(serialize(x)) }).asInstanceOf[Transporter.RType] }
}

given Liftable[TypeMemberInfo] {
  def toExpr(x: TypeMemberInfo) =
    '{ new TypeMemberInfo(${Expr(x.name)}, ${Expr(x.typeSymbol)}, ${ Expr(x.memberType) } ) }
}

given Liftable[SelfRefRType] {
  def toExpr(x: SelfRefRType) =
    '{ new SelfRefRType(${Expr(x.name)}) }
}

// In order to cross the compiler->runtime bridge, we need to serialize some objects, e.g. traits.
// Then on the runtime side we deserialize them back into objects again.
inline def serialize(o: Object): String = 
  val baos = new ByteArrayOutputStream()
  val oos  = new ObjectOutputStream(baos)
  oos.writeObject(o)
  val bytes = Base64.getEncoder.encodeToString(baos.toByteArray)
  baos.close
  oos.close
  bytes

inline def deserialize(b: String): Object = 
  val bytes = Base64.getDecoder.decode(b)
  val bais = new ByteArrayInputStream(bytes)
  val ois  = new ObjectInputStream(bais)
  val ret = ois.readObject()
  bais.close
  ois.close
  ret
