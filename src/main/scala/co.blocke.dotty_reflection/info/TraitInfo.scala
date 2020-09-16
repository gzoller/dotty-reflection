package co.blocke.dotty_reflection
package info

import scala.tasty.Reflection
import impl._
import Transporter.AppliedRType
import java.nio.ByteBuffer


object TraitInfo:
  def fromBytes( bbuf: ByteBuffer ): TraitInfo = 
    TraitInfo(
      StringByteEngine.read(bbuf),
      ArrayByteEngine[FieldInfo](FieldInfoByteEngine).read(bbuf),
      ArrayByteEngine[Transporter.RType](RTypeByteEngine).read(bbuf),
      ArrayByteEngine[String](StringByteEngine).read(bbuf).map(_.asInstanceOf[TypeSymbol])
      )

case class TraitInfo protected[dotty_reflection](
    name: String, 
    fields: Array[FieldInfo],
    actualParameterTypes: Array[Transporter.RType] = Array.empty[Transporter.RType],
    paramSymbols: Array[TypeSymbol] = Array.empty[TypeSymbol],
  ) extends Transporter.RType with Transporter.AppliedRType: 

  val fullName: String = 
    if actualParameterTypes.size > 0 then
      name + actualParameterTypes.map(_.fullName).toList.mkString("[",",","]")
    else
      name
  lazy val infoClass: Class[_] = Class.forName(name)
 
  override def isAppliedType: Boolean = paramSymbols.nonEmpty

  override def resolveTypeParams( paramMap: Map[TypeSymbol, Transporter.RType] ): Transporter.RType =
    TraitInfo(
      name, 
      fields.map( _.asInstanceOf[ScalaFieldInfo].resolveTypeParams(paramMap) ),
      actualParameterTypes.map( _ match {
          case ts: TypeSymbolInfo if paramMap.contains(ts.name.asInstanceOf[TypeSymbol]) => paramMap(ts.name.asInstanceOf[TypeSymbol])
          case art: AppliedRType if art.isAppliedType => art.resolveTypeParams(paramMap)
          case t => t
        }),
      paramSymbols
      )

  override def findPaths(findSyms: Map[TypeSymbol,Path], referenceTrait: Option[TraitInfo] = None): (Map[TypeSymbol, Path], Map[TypeSymbol, Path]) = 
    val interestingFields = referenceTrait match {
      case Some(t:TraitInfo) => fields.filter(f => t.fields.map(_.name).contains(f.name))
      case _ => fields
    }
    interestingFields.foldLeft((Map.empty[TypeSymbol,Path], findSyms)) { (acc, f) =>
      val (found, notFound) = acc
      if notFound.nonEmpty then
        f.fieldType match {
          case ts: TypeSymbolInfo if notFound.contains(ts.name.asInstanceOf[TypeSymbol]) =>
            // This field's type is one of the sought-after TypeSymbols...
            val sym = ts.name.asInstanceOf[TypeSymbol]
            (found + (sym -> notFound(sym).push(TraitPathElement(name,f.name))), notFound - sym)
          case _ =>
            // Or it's not...
            val (themThatsFound, themThatsStillLost) = f.fieldType.findPaths(notFound.map( (k,v) => k -> v.push(TraitPathElement(name,f.name)) ))
            (found ++ themThatsFound, themThatsStillLost.map( (k,v) => k -> findSyms(k) ))
        }
      else
        (found, notFound)
      }

  override def toType(reflect: Reflection): reflect.Type = 
    import reflect.{_, given _}
    if actualParameterTypes.nonEmpty then
      val args = actualParameterTypes.map(_.toType(reflect).asInstanceOf[reflect.Type]).toList
      AppliedType(Type(infoClass), args)
    else
      reflect.Type(infoClass)

  def show(tab: Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}

    if seenBefore.contains(name) then
      {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName + s"($name) (self-ref recursion)\n"
    else
      val params = 
        if actualParameterTypes.isEmpty then 
          "" 
        else 
          val syms = actualParameterTypes.zip(paramSymbols)
          " actualParamTypes: [\n"+syms.map{ (ap:Transporter.RType, s:TypeSymbol) => tabs(tab+1) + s.toString+": "+ap.show(tab+2,name :: seenBefore, true) }.mkString + tabs(tab) + "]"
      {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName 
      + s"($name)$params with fields:\n"
      + { fields.toList.map(f => tabs(tab+1)+f.name+{if f.originalSymbol.isDefined then "["+f.originalSymbol.get.toString+"]" else ""}+": "+f.fieldType.show(tab+1, Nil, true)).mkString("") }

  def toBytes( bbuf: ByteBuffer ): Unit = 
    bbuf.put( TRAIT_INFO )
    StringByteEngine.write(bbuf, name)
    ArrayByteEngine[FieldInfo](FieldInfoByteEngine).write(bbuf, fields)
    ArrayByteEngine[Transporter.RType](RTypeByteEngine).write(bbuf, actualParameterTypes)
    ArrayByteEngine[String](StringByteEngine).write(bbuf, paramSymbols.asInstanceOf[Array[String]])

//------------------------------------------------------------

object SealedTraitInfo:
  def fromBytes( bbuf: ByteBuffer ): SealedTraitInfo = 
    SealedTraitInfo(
      StringByteEngine.read(bbuf),
      ArrayByteEngine[Transporter.RType](RTypeByteEngine).read(bbuf)
      )

case class SealedTraitInfo protected(
    name: String, 
    children: Array[Transporter.RType]
  ) extends Transporter.RType:

  val fullName: String = name + children.map(_.fullName).toList.mkString("[",",","]")
  lazy val infoClass: Class[_] = Class.forName(name)

  def show(tab: Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    if seenBefore.contains(name) then
      {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName + s"($name) (self-ref recursion)\n"
    else
      {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName 
      + s"($name)"
      + {if children.isEmpty then "\n" else ":\n"+ tabs(newTab) + "children:\n" + children.map(_.show(newTab+1,name :: seenBefore)).mkString}

  def toBytes( bbuf: ByteBuffer ): Unit = 
    bbuf.put( SEALED_TRAIT_INFO )
    StringByteEngine.write(bbuf, name)
    ArrayByteEngine[Transporter.RType](RTypeByteEngine).write(bbuf, children)
    
  