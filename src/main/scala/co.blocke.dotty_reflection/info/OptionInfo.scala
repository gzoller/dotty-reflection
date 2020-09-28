package co.blocke.dotty_reflection
package info

import java.lang.reflect._
import java.util.Optional
import impl._
import scala.tasty.Reflection
import java.nio.ByteBuffer


trait OptionInfo extends RType with AppliedRType:
  lazy val optionParamType: RType

  def select(i: Int): RType = 
    if i == 0 then
      optionParamType
    else
      throw new ReflectException(s"AppliedType select index $i out of range for ${name}")
      


object ScalaOptionInfo:
  def fromBytes( bbuf: ByteBuffer ): ScalaOptionInfo =
    ScalaOptionInfo(
      StringByteEngine.read(bbuf),
      RTypeByteEngine.read(bbuf)
      )

//-------------------

case class ScalaOptionInfo protected[dotty_reflection](
  name: String,
  _optionParamType: RType
) extends OptionInfo:

  val fullName: String = name + "[" + _optionParamType.fullName  + "]"
  lazy val infoClass: Class[_] = Class.forName(name)
  lazy val optionParamType: RType = _optionParamType match {
    case e: SelfRefRType => e.resolve
    case e => e
  }

  def resolveTypeParams( paramMap: Map[TypeSymbol, RType] ): RType = 
    _optionParamType match {
      case ts: TypeSymbolInfo if paramMap.contains(ts.name.asInstanceOf[TypeSymbol]) => ScalaOptionInfo(name, paramMap(ts.name.asInstanceOf[TypeSymbol]))
      case art: AppliedRType if art.isAppliedType => ScalaOptionInfo(name, art.resolveTypeParams(paramMap))
      case _ => this
    }
    
  def show(tab: Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + "Option of " + optionParamType.show(newTab,name :: seenBefore,true)

  def toBytes( bbuf: ByteBuffer ): Unit = 
    bbuf.put( OPTION_INFO )
    StringByteEngine.write(bbuf, name)
    RTypeByteEngine.write(bbuf, _optionParamType)


object JavaOptionalInfo:
  def fromBytes( bbuf: ByteBuffer ): JavaOptionalInfo =
    JavaOptionalInfo(
      StringByteEngine.read(bbuf),
      RTypeByteEngine.read(bbuf)
      )

//-------------------
      
case class JavaOptionalInfo protected[dotty_reflection](
  name: String,
  _optionParamType: RType
) extends OptionInfo:

  val fullName: String = name + "[" + _optionParamType.fullName  + "]"
  lazy val infoClass: Class[_] = Class.forName(name)
  lazy val optionParamType: RType = _optionParamType match {
    case e: SelfRefRType => e.resolve
    case e => e
  }
   
  def show(tab: Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + "Optional of " + optionParamType.show(newTab,name :: seenBefore,true)

  override def resolveTypeParams( paramMap: Map[TypeSymbol, RType] ): RType = 
    _optionParamType match {
      case ts: TypeSymbolInfo if paramMap.contains(ts.name.asInstanceOf[TypeSymbol]) => JavaOptionalInfo(name, paramMap(ts.name.asInstanceOf[TypeSymbol]))
      case art: AppliedRType if art.isAppliedType => JavaOptionalInfo(name, art.resolveTypeParams(paramMap))
      case _ => this
    }

  def toBytes( bbuf: ByteBuffer ): Unit = 
    bbuf.put( OPTIONAL_INFO )
    StringByteEngine.write(bbuf, name)
    RTypeByteEngine.write(bbuf, _optionParamType)