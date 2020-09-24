package co.blocke.dotty_reflection
package info

import impl._
import scala.tasty.Reflection
import java.nio.ByteBuffer

/** Arity 1 Collections, e.g. List, Set, Seq */
object SeqLikeInfo:
  def fromBytes( bbuf: ByteBuffer ): SeqLikeInfo =
    SeqLikeInfo(
      StringByteEngine.read(bbuf),
      RTypeByteEngine.read(bbuf)
      )

case class SeqLikeInfo protected[dotty_reflection](
  name: String,
  _elementType: RType,
) extends RType with CollectionRType:

  val fullName: String = name + "[" + _elementType.fullName + "]"
  lazy val infoClass: Class[_] = Class.forName(name)
      
  override def resolveTypeParams( paramMap: Map[TypeSymbol, RType] ): RType =
    _elementType match {
      case ts: TypeSymbolInfo if paramMap.contains(ts.name.asInstanceOf[TypeSymbol]) => 
        SeqLikeInfo(name, paramMap(ts.name.asInstanceOf[TypeSymbol]))
      case art: AppliedRType if art.isAppliedType => 
        SeqLikeInfo(name, _elementType.resolveTypeParams(paramMap))
      case _ => this
    }

  lazy val elementType: RType = _elementType match {
    case e: SelfRefRType => e.resolve
    case e => e
  }

  def toBytes( bbuf: ByteBuffer ): Unit = 
    bbuf.put( SEQLIKE_INFO )
    StringByteEngine.write(bbuf, name)
    RTypeByteEngine.write(bbuf, _elementType)


/** Arity 2 Collections, Map flavors, basiclly */
object MapLikeInfo:
  def fromBytes( bbuf: ByteBuffer ): MapLikeInfo =
    MapLikeInfo(
      StringByteEngine.read(bbuf),
      RTypeByteEngine.read(bbuf),
      RTypeByteEngine.read(bbuf)
      )

case class MapLikeInfo protected[dotty_reflection](
  name: String,
  _elementType: RType,
  _elementType2: RType
) extends RType with CollectionRType:

  val fullName = name + "[" + _elementType.fullName + "," + _elementType2.fullName + "]"
  lazy val infoClass: Class[_] = Class.forName(name)

  override def findPaths(findSyms: Map[TypeSymbol,Path], referenceTrait: Option[TraitInfo] = None): (Map[TypeSymbol, Path], Map[TypeSymbol, Path]) = 
    val (stage1Found, stage1Unfound) = elementType match {
      case ts: TypeSymbolInfo if findSyms.contains(ts.name.asInstanceOf[TypeSymbol]) =>
        val sym = ts.name.asInstanceOf[TypeSymbol]
        (Map( ts.name.asInstanceOf[TypeSymbol] -> findSyms(sym).add(Path.MAP_KEY_PATH).lock ), findSyms - sym)
      case other => 
        other.findPaths(findSyms.map( (k,v) => k -> v.fork.add(Path.MAP_KEY_PATH) ))
    }
    val (stage2Found, stage2Unfound) = elementType2 match {
      case ts: TypeSymbolInfo if stage1Unfound.contains(ts.name.asInstanceOf[TypeSymbol]) =>
        val sym = ts.name.asInstanceOf[TypeSymbol]
        (Map( ts.name.asInstanceOf[TypeSymbol] -> stage1Unfound(sym).add(Path.MAP_VALUE_PATH).lock ), findSyms - sym)
      case other => 
        other.findPaths(stage1Unfound.map( (k,v) => k -> v.fork.add(Path.MAP_VALUE_PATH) ))
    }
    (stage1Found ++ stage2Found, stage2Unfound)
    
  override def resolveTypeParams( paramMap: Map[TypeSymbol, RType] ): RType = 
    val stage1 = _elementType match {
      case ts: TypeSymbolInfo if paramMap.contains(ts.name.asInstanceOf[TypeSymbol]) => 
        MapLikeInfo(name, paramMap(ts.name.asInstanceOf[TypeSymbol]), _elementType2)
      case art: AppliedRType if art.isAppliedType => 
        MapLikeInfo(name, _elementType.resolveTypeParams(paramMap), _elementType2)
      case _ => this
    }
    _elementType2 match {
      case ts: TypeSymbolInfo if paramMap.contains(ts.name.asInstanceOf[TypeSymbol]) => 
        MapLikeInfo(name, stage1._elementType, paramMap(ts.name.asInstanceOf[TypeSymbol]))
      case art: AppliedRType if art.isAppliedType => 
        MapLikeInfo(name, stage1._elementType, _elementType2.resolveTypeParams(paramMap))
      case _ => stage1
    }

  lazy val elementType: RType = _elementType match {
    case e: SelfRefRType => e.resolve
    case e => e
  }
  lazy val elementType2: RType = _elementType2 match {
    case e: SelfRefRType => e.resolve
    case e => e
  }

  override def show(tab: Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName 
    + s"($name):\n"
    + elementType.show(newTab)
    + elementType2.show(newTab)

  def toBytes( bbuf: ByteBuffer ): Unit = 
    bbuf.put( MAPLIKE_INFO )
    StringByteEngine.write(bbuf, name)
    RTypeByteEngine.write(bbuf, _elementType)
    RTypeByteEngine.write(bbuf, _elementType2)


/** Scala Array */
object ArrayInfo:
  def fromBytes( bbuf: ByteBuffer ): ArrayInfo =
    ArrayInfo(
      StringByteEngine.read(bbuf),
      RTypeByteEngine.read(bbuf)
      )

case class ArrayInfo protected[dotty_reflection](
  name: String,
  _elementType: RType
) extends RType with CollectionRType:

  val fullName = name + "[" + _elementType.fullName + "]"
  lazy val infoClass: Class[_] = Class.forName(name)
      
  override def resolveTypeParams( paramMap: Map[TypeSymbol, RType] ): RType = 
    _elementType match {
      case ts: TypeSymbolInfo if paramMap.contains(ts.name.asInstanceOf[TypeSymbol]) => 
        ArrayInfo(name, paramMap(ts.name.asInstanceOf[TypeSymbol]))
      case art: AppliedRType if art.isAppliedType => 
        ArrayInfo(name, _elementType.resolveTypeParams(paramMap))
      case _ => this
    }

  lazy val elementType: RType = _elementType match {
    case e: SelfRefRType => e.resolve
    case e => e
  }

  override def show(tab: Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + s"array of " + elementType.show(newTab,name :: seenBefore,true)
  
  def toBytes( bbuf: ByteBuffer ): Unit = 
    bbuf.put( ARRAY_INFO )
    StringByteEngine.write(bbuf, name)
    RTypeByteEngine.write(bbuf, _elementType)


/** Java Set dirivative */
object JavaSetInfo:
  def fromBytes( bbuf: ByteBuffer ): JavaSetInfo =
    JavaSetInfo(
      StringByteEngine.read(bbuf),
      RTypeByteEngine.read(bbuf)
      )

case class JavaSetInfo protected[dotty_reflection](
  name: String,
  _elementType: RType
) extends RType with CollectionRType:

  val fullName = name + "[" + _elementType.fullName + "]"
  lazy val infoClass: Class[_] = Class.forName(name)
      
  override def resolveTypeParams( paramMap: Map[TypeSymbol, RType] ): RType = 
    _elementType match {
      case ts: TypeSymbolInfo if paramMap.contains(ts.name.asInstanceOf[TypeSymbol]) => 
        JavaSetInfo(name, paramMap(ts.name.asInstanceOf[TypeSymbol]))
      case art: AppliedRType if art.isAppliedType => 
        JavaSetInfo(name, _elementType.resolveTypeParams(paramMap))
      case _ => this
    }

  lazy val elementType: RType = _elementType match {
    case e: SelfRefRType => e.resolve
    case e => e
  }
    
  def toBytes( bbuf: ByteBuffer ): Unit = 
    bbuf.put( JAVA_SET_INFO )
    StringByteEngine.write(bbuf, name)
    RTypeByteEngine.write(bbuf, _elementType)


/** Java List dirivative */
object JavaListInfo:
  def fromBytes( bbuf: ByteBuffer ): JavaListInfo =
    JavaListInfo(
      StringByteEngine.read(bbuf),
      RTypeByteEngine.read(bbuf)
      )

case class JavaListInfo protected[dotty_reflection](
  name: String,
  _elementType: RType
) extends RType with CollectionRType:

  val fullName = name + "[" + _elementType.fullName + "]"
  lazy val infoClass: Class[_] = Class.forName(name)
      
  override def resolveTypeParams( paramMap: Map[TypeSymbol, RType] ): RType = 
    _elementType match {
      case ts: TypeSymbolInfo if paramMap.contains(ts.name.asInstanceOf[TypeSymbol]) => 
        JavaListInfo(name, paramMap(ts.name.asInstanceOf[TypeSymbol]))
      case art: AppliedRType if art.isAppliedType => 
        JavaListInfo(name, _elementType.resolveTypeParams(paramMap))
      case _ => this
    }

  lazy val elementType: RType = _elementType match {
    case e: SelfRefRType => e.resolve
    case e => e
  }
     
  def toBytes( bbuf: ByteBuffer ): Unit = 
    bbuf.put( JAVA_LIST_INFO )
    StringByteEngine.write(bbuf, name)
    RTypeByteEngine.write(bbuf, _elementType)


/** Java Array */
object JavaArrayInfo:
  def fromBytes( bbuf: ByteBuffer ): JavaArrayInfo =
    JavaArrayInfo(
      StringByteEngine.read(bbuf),
      RTypeByteEngine.read(bbuf)
      )

case class JavaArrayInfo protected[dotty_reflection](
  name: String,
  _elementType: RType
) extends RType with CollectionRType:
 
  val fullName = name + "[" + _elementType.fullName + "]"
  lazy val infoClass: Class[_] = Class.forName(name)
      
  override def resolveTypeParams( paramMap: Map[TypeSymbol, RType] ): RType = 
    _elementType match {
      case ts: TypeSymbolInfo if paramMap.contains(ts.name.asInstanceOf[TypeSymbol]) => 
        JavaArrayInfo(name, paramMap(ts.name.asInstanceOf[TypeSymbol]))
      case art: AppliedRType if art.isAppliedType => 
        JavaArrayInfo(name, _elementType.resolveTypeParams(paramMap))
      case _ => this
    }

  lazy val elementType: RType = _elementType match {
    case e: SelfRefRType => e.resolve
    case e => e
  }

  override def show(tab: Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + s"array of " + elementType.show(newTab,name :: seenBefore,true)
     
  def toBytes( bbuf: ByteBuffer ): Unit = 
    bbuf.put( JAVA_ARRAY_INFO )
    StringByteEngine.write(bbuf, name)
    RTypeByteEngine.write(bbuf, _elementType)


/** Java Queue dirivative */
object JavaQueueInfo:
  def fromBytes( bbuf: ByteBuffer ): JavaQueueInfo =
    JavaQueueInfo(
      StringByteEngine.read(bbuf),
      RTypeByteEngine.read(bbuf)
      )

case class JavaQueueInfo protected[dotty_reflection](
  name: String,
  _elementType: RType
) extends RType with CollectionRType:

  val fullName = name + "[" + _elementType.fullName + "]"
  lazy val infoClass: Class[_] = Class.forName(name)
      
  override def resolveTypeParams( paramMap: Map[TypeSymbol, RType] ): RType = 
    _elementType match {
      case ts: TypeSymbolInfo if paramMap.contains(ts.name.asInstanceOf[TypeSymbol]) => 
        JavaQueueInfo(name, paramMap(ts.name.asInstanceOf[TypeSymbol]))
      case art: AppliedRType if art.isAppliedType => 
        JavaQueueInfo(name, _elementType.resolveTypeParams(paramMap))
      case _ => this
    }

  lazy val elementType: RType = _elementType match {
    case e: SelfRefRType => e.resolve
    case e => e
  }
     
  def toBytes( bbuf: ByteBuffer ): Unit = 
    bbuf.put( JAVA_QUEUE_INFO )
    StringByteEngine.write(bbuf, name)
    RTypeByteEngine.write(bbuf, _elementType)


/** Java Stack dirivative */
object JavaStackInfo:
  def fromBytes( bbuf: ByteBuffer ): JavaStackInfo =
    JavaStackInfo(
      StringByteEngine.read(bbuf),
      RTypeByteEngine.read(bbuf)
      )

case class JavaStackInfo protected[dotty_reflection](
  name: String,
  _elementType: RType
) extends RType with CollectionRType:

  val fullName = name + "[" + _elementType.fullName + "]"
  lazy val infoClass: Class[_] = Class.forName(name)
      
  override def resolveTypeParams( paramMap: Map[TypeSymbol, RType] ): RType = 
    _elementType match {
      case ts: TypeSymbolInfo if paramMap.contains(ts.name.asInstanceOf[TypeSymbol]) => 
        JavaStackInfo(name, paramMap(ts.name.asInstanceOf[TypeSymbol]))
      case art: AppliedRType if art.isAppliedType => 
        JavaStackInfo(name, _elementType.resolveTypeParams(paramMap))
      case _ => this
    }

  lazy val elementType: RType = _elementType match {
    case e: SelfRefRType => e.resolve
    case e => e
  }
       
  def toBytes( bbuf: ByteBuffer ): Unit = 
    bbuf.put( JAVA_STACK_INFO )
    StringByteEngine.write(bbuf, name)
    RTypeByteEngine.write(bbuf, _elementType)


/** Java Map dirivative */
object JavaMapInfo:
  def fromBytes( bbuf: ByteBuffer ): JavaMapInfo =
    JavaMapInfo(
      StringByteEngine.read(bbuf),
      RTypeByteEngine.read(bbuf),
      RTypeByteEngine.read(bbuf)
      )

case class JavaMapInfo protected[dotty_reflection](
  name: String,
  _elementType: RType,
  _elementType2: RType
) extends RType with CollectionRType:

  val fullName = name + "[" + _elementType.fullName + "," + _elementType2.fullName + "]"
  lazy val infoClass: Class[_] = Class.forName(name)
  
  lazy val elementType: RType = _elementType match {
    case e: SelfRefRType => e.resolve
    case e => e
  }
  lazy val elementType2: RType = _elementType2 match {
    case e: SelfRefRType => e.resolve
    case e => e
  }

  override def findPaths(findSyms: Map[TypeSymbol,Path], referenceTrait: Option[TraitInfo] = None): (Map[TypeSymbol, Path], Map[TypeSymbol, Path]) = 
    val (stage1Found, stage1Unfound) = elementType match {
      case ts: TypeSymbolInfo if findSyms.contains(ts.name.asInstanceOf[TypeSymbol]) =>
        val sym = ts.name.asInstanceOf[TypeSymbol]
        (Map( ts.name.asInstanceOf[TypeSymbol] -> findSyms(sym).add(Path.MAP_KEY_PATH).lock ), findSyms - sym)
      case other => 
        other.findPaths(findSyms.map( (k,v) => k -> v.fork.add(Path.MAP_KEY_PATH) ))
    }
    val (stage2Found, stage2Unfound) = elementType2 match {
      case ts: TypeSymbolInfo if stage1Unfound.contains(ts.name.asInstanceOf[TypeSymbol]) =>
        val sym = ts.name.asInstanceOf[TypeSymbol]
        (Map( ts.name.asInstanceOf[TypeSymbol] -> stage1Unfound(sym).add(Path.MAP_VALUE_PATH).lock ), findSyms - sym)
      case other => 
        other.findPaths(stage1Unfound.map( (k,v) => k -> v.fork.add(Path.MAP_VALUE_PATH) ))
    }
    (stage1Found ++ stage2Found, stage2Unfound)

  override def resolveTypeParams( paramMap: Map[TypeSymbol, RType] ): RType = 
    val stage1 = _elementType match {
      case ts: TypeSymbolInfo if paramMap.contains(ts.name.asInstanceOf[TypeSymbol]) => 
        JavaMapInfo(name, paramMap(ts.name.asInstanceOf[TypeSymbol]), _elementType2)
      case art: AppliedRType if art.isAppliedType => 
        JavaMapInfo(name, _elementType.resolveTypeParams(paramMap), _elementType2)
      case _ => this
    }
    _elementType2 match {
      case ts: TypeSymbolInfo if paramMap.contains(ts.name.asInstanceOf[TypeSymbol]) => 
        JavaMapInfo(name, stage1._elementType, paramMap(ts.name.asInstanceOf[TypeSymbol]))
      case art: AppliedRType if art.isAppliedType => 
        JavaMapInfo(name, stage1._elementType, _elementType2.resolveTypeParams(paramMap))
      case _ => stage1
    }
  
  override def show(tab: Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName 
    + s"($name):\n"
    + elementType.show(newTab,name :: seenBefore)
    + elementType2.show(newTab,name :: seenBefore)
      
  def toBytes( bbuf: ByteBuffer ): Unit = 
    bbuf.put( JAVA_MAP_INFO )
    StringByteEngine.write(bbuf, name)
    RTypeByteEngine.write(bbuf, _elementType)
    RTypeByteEngine.write(bbuf, _elementType2)