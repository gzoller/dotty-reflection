package co.blocke.dotty_reflection
package impl

import scala.tasty.Reflection

// import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable.{Map => MMap}

/** We need to record a A->B type parameter graph.  So for:
 *
 *  trait A[T]
 *  case class B[U]() extends A[U]
 *
 *  We need to record that for the relationship A -> B that param T maps to U.
 */
trait ParamGraph:
  self: ScalaClassInspector =>

  private val graph = MMap.empty[RType, MMap[RType, Map[TypeSymbol,TypeSymbol]]]

  private inline def add(parent: RType, child: RType, paramMap: Map[TypeSymbol, TypeSymbol]) =
    if graph.contains(parent) then
      graph(parent).put(child, paramMap)
    else
      graph.put(parent, MMap(child -> paramMap))

  protected def retister(reflect: Reflection, paramMap: Map[TypeSymbol,RType])(child: RType, parentTref: reflect.TypeRef) =
    if child.orderedTypeParameters.nonEmpty then
      val dadsRtype = inspectType(reflect, paramMap)(parentTref)
      if dadsRtype.orderedTypeParameters.nonEmpty then
        println("Parent: "+dadsRtype)
        println("----")
        println("Child:  "+child)


      /*
  trait A[T]
  trait B[Q] extends A[Q]
  case class C[U]() extends B[U]


  1. Figure out ecosystem of C -> Set(E)
  2. Associate parent to child C -> for each E :> U -> E[_]
       B -> C, (Q -> U)
  3. Recurse on E:
       A -> B, (T -> Q)
  4. Now for each level, expand the mapping to the floor:
       A -> C, (T -> U)


       private def descendInto(className: String, reflect: Reflection, paramMap: Map[TypeSymbol,RType])(tree: reflect.Tree): Option[RType] =
        println(inspectType(reflect, paramMap)(a.tpe.asInstanceOf[reflect.TypeRef]))
      */