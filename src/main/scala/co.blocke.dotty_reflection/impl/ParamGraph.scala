package co.blocke.dotty_reflection
package impl

import info._
// import scala.quoted._
// import scala.reflect._
import scala.tasty.Reflection

// import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable.{Map => MMap, Set => MSet}

/** This registry is to support ScalaJack's need to express a concrete class in terms of a trait, which is an ancestor of the class:
 *
 * trait Pet[X]
 * case class Dog[X](a:Y) extends Pet[Y]
 *
 * We need to be able to map the relationship between parent/child and map their type parameters so the correct substitutions
 * can be made.
 */
object ParamGraphRegistry:

  // Parent -> Children -> mapping
  private val graph = MMap.empty[Class[_], MMap[Class[_], Map[TypeSymbol,TypeSymbol]]]

  // Child -> parent list
  private val child2dadAssoc = MMap.empty[Class[_],MSet[Class[_]]] // basically a reverse of graph

  def add( parents: List[(Class[_], Map[TypeSymbol,TypeSymbol])], child: Class[_] ): Unit =
    parents.map{ (parent,symbolMap) =>
      if graph.contains(parent) then {
        if( !graph(parent).contains(child) ) 
          graph(parent).put(child, symbolMap)  // don't dupliate children if exists
      } else
        graph.put(parent, MMap(child -> symbolMap))
      
      if(child2dadAssoc.contains(child)) {
        if( !child2dadAssoc(child).contains(parent) )
          child2dadAssoc.put(child, child2dadAssoc(child) += parent)
      } else
        child2dadAssoc.put(child,MSet(parent))
      }

    parents.map { (p,m) => 
      child2dadAssoc.get(p) match {
        case Some(x) =>
          val wired = x.map { grandparent =>
            val grandpa2parentMap = graph(grandparent)(p)
            val parent2childMap = graph(p)(child)
            (grandparent, grandpa2parentMap.keySet.collect {
              case s if parent2childMap.contains(grandpa2parentMap(s)) => s -> parent2childMap(grandpa2parentMap(s))
            }.toMap)
          }
          add(wired.toList, child)
        case _ =>
      }
    }

  // def resolveTypesFor(parent: RType, child: RType): Option[List[RType]] =
  //   graph.get(parent).map( _.get(child).map{ parmMap =>
  //     child.orderedTypeParameters
  //   })


  def show: String = 
    graph.keySet.map(k => s"$k:\n" + graph(k).keySet.map(k2 => s"    $k2: "+graph(k)(k2)).mkString("\n")).mkString("\n")


trait ParamGraph:
  self: ScalaClassInspector =>

  protected def registerParents(reflect: Reflection, paramMap: Map[TypeSymbol,RType])(t: reflect.ClassDef, classInfo: RType): Unit = //ScalaClassInfo) =
    import reflect.{_, given _}
    if classInfo.orderedTypeParameters.nonEmpty then
      val parentTrees: List[dotty.tools.dotc.ast.Trees.AppliedTypeTree[_]] = t.parents.collect {
        case a: dotty.tools.dotc.ast.Trees.AppliedTypeTree[_] => a // This matches trait mixins--our primary target
      }
      val parents = parentTrees.map{ appliedTypeTree =>
        val parentRtype = inspectType(reflect, Map.empty[TypeSymbol,RType])(appliedTypeTree.tpe.asInstanceOf[reflect.TypeRef])

        // This seemingly redundant call forces us to look at dad's ancestors in terms of dad; in other words reflect on dad's parentage up the tree
        // recursively.  (NOTE: We'll then need to expand the linkages (type associations) down the tree to the leaf nodes/classes!)
        Reflector.reflectOnClass( parentRtype.infoClass )

        parentRtype
      }
      if(parents.nonEmpty)
        registerTypeMap(reflect, paramMap)(classInfo, parents)
  

  private def registerTypeMap(reflect: Reflection, paramMap: Map[TypeSymbol,RType])(child: RType, parents: List[RType]) =
    val candidates = parents.collect {
      case parent: TraitInfo if parent.orderedTypeParameters.nonEmpty => 
        val traitParamMap = parent.orderedTypeParameters.zip(parent.actualParameterTypes).collect{case (sym,v:TypeSymbolInfo) => (sym, v.name.asInstanceOf[TypeSymbol]) }.toMap
        (parent.infoClass, traitParamMap)
      }.toList
    ParamGraphRegistry.add( candidates, child.infoClass )