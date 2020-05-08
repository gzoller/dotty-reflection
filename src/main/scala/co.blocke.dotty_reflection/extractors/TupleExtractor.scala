package co.blocke.dotty_reflection
package extractors

import impl._
import Clazzes._
import info._ 
import scala.tasty.Reflection
import scala.util.matching.Regex

case class TupleExtractor() extends TypeInfoExtractor[TupleInfo]:

  val tupleFullName: Regex = """scala.Tuple(\d+)""".r

  def matches(clazz: Class[_]): Boolean = tupleFullName.matches(clazz.getName)

  def emptyInfo(clazz: Class[_], paramMap: Map[TypeSymbol,RType]): TupleInfo = 
    val tupleTypeSyms = clazz.getTypeParameters.toList.map(_.getName)
    val tupleParamTypes = tupleTypeSyms.map( et => paramMap.getOrElse(
      et.asInstanceOf[TypeSymbol], 
      TypeSymbolInfo(et)
      ))
    TupleInfo(
      clazz.getName, 
      clazz, 
      tupleParamTypes)

  def extractInfo(reflect: Reflection, paramMap: Map[TypeSymbol,RType])(
    t: reflect.Type, 
    tob: List[reflect.TypeOrBounds], 
    className: String, 
    clazz: Class[_], 
    typeInspector: ScalaClassInspectorLike): RType =

    val elementTypes = tob.map( t => typeInspector.inspectType(reflect, paramMap)(t.asInstanceOf[reflect.TypeRef]))
    // val (elementTypes, elementTypeSymbols) = tob.foldRight( (List.empty[ALL_TYPE], List.empty[Option[TypeSymbol]]) ){ (rawTypeRef, acc) => 
    // typeInspector.inspectType(reflect)(rawTypeRef.asInstanceOf[reflect.TypeRef]) match {
    //   case ts: TypeSymbol =>   (acc._1 :+ PrimitiveType.Scala_Any, acc._2 :+ Some(ts))
    //   case ct: ConcreteType => (acc._1 :+ ct, acc._2 :+ None)
    //   }
    // }
    TupleInfo(className, clazz, elementTypes)
