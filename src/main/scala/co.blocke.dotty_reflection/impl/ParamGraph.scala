package co.blocke.dotty_reflection
package impl

import info._
import scala.tasty.Reflection

import scala.collection.mutable.{Map => MMap, Set => MSet}


object ParamCache:

  // Parent -> (Child -> ((child) TypeSymbol -> SymPath))
  private val pcache = MMap.empty[Class[_], MMap[Class[_], Map[TypeSymbol, SymPath]]]

  // Child -> List[Parent]
  private val child2dadAssoc = MMap.empty[Class[_], MSet[Class[_]]]

  def contains(parent: Class[_], child: Class[_]): Boolean = pcache.get(parent).flatMap(_.get(child)).isDefined

  def add( parent: Class[_], child: Class[_], maps: Map[TypeSymbol, SymPath]): Unit =
    if( !pcache.contains(parent) )
      pcache.put(parent, MMap(child -> maps))
    else
      pcache(parent).put(child, maps)
    if(child2dadAssoc.contains(child)) {
      if( !child2dadAssoc(child).contains(parent) )
        child2dadAssoc.put(child, child2dadAssoc(child) += parent)
    } else
      child2dadAssoc.put(child,MSet(parent))

    child2dadAssoc.get(parent) match {
      case Some(x) =>
        x.map { grandparent =>
          val grandpa2parentMap = pcache(grandparent)(parent)
          val parent2childMap = pcache(parent)(child)
          // println(s">> Register ${child.getSimpleName} with parent ${parent.getSimpleName} having grandpa ${grandparent.getSimpleName}")
          // println(s"grandpa2parentMap = $grandpa2parentMap")
          // println(s"parent2childMap   = $parent2childMap")
          // println("---")
          val grandMap = parent2childMap.keySet.collect {
            case s if grandpa2parentMap.contains(parent2childMap(s).parentSym) => s -> { grandpa2parentMap(parent2childMap(s).parentSym) match {
              case n: PathNode => n.copy( childSym = s )
              case n: PathBranch => fixBranch(n, s)
            }}
          }.toMap
          if grandMap.nonEmpty then
            add(grandparent, child, grandMap)
        }
      case _ =>
    }


  // Drive to leaf and replace child symbol with s
  private def fixBranch( b: PathBranch, s: TypeSymbol ): PathBranch =
    b.branch match {
      case n: PathNode => b.copy(branch = n.copy( childSym = s ))
      case b2: PathBranch => b.copy( branch = fixBranch( b2, s ))
    }


  def resolveTypesFor(parent: TraitInfo, child: RType): Option[List[RType]] =
    val foundChild: Option[Map[TypeSymbol, SymPath]] = pcache.get(parent.infoClass).flatMap(_.get(child.infoClass))
    foundChild.map( mappings => 
      child.orderedTypeParameters.map( param => mappings.get(param).match {
        case Some(path) => path.resolve(parent)
        case _ => TypeSymbolInfo(param.toString) // mapping not found--take your best guess!
      })
    )


trait ParamGraph:
  self: ScalaClassInspectorLike =>

  protected def registerParents(reflect: Reflection)(t: reflect.ClassDef, classInfo: RType): Unit = //ScalaCaseClassInfo) =
    import reflect.{_, given _}
    if classInfo.orderedTypeParameters.nonEmpty then
      t.parents.collect {
        case a: dotty.tools.dotc.ast.Trees.AppliedTypeTree[_] =>  // This matches trait mixins--our primary target
          val className = a.tpe.asInstanceOf[reflect.AppliedType].tycon.asInstanceOf[reflect.TypeRef].typeSymbol.fullName
          if !ParamCache.contains(Class.forName(className), classInfo.infoClass) then
            // Get the RType of each parent trait
            val parentRType = inspectType(reflect, Map.empty[TypeSymbol,RType])(a.tpe.asInstanceOf[reflect.TypeRef])
            // For each parent, dive in and find paths to type symbols
            val pathsForParent = unpackSymbolPaths( classInfo.orderedTypeParameters, parentRType ).toMap
            // register paths for this parent
            ParamCache.add(parentRType.infoClass, classInfo.infoClass, pathsForParent)
        }

  private def unpackSymbolPaths( syms: List[TypeSymbol], parent: RType ): List[(TypeSymbol,SymPath)] =
    parent match {
      case p: TraitInfo =>
        p.typedParams.flatMap( (k,v) => v match {
          case s: TypeSymbolInfo if syms.contains(s.name.asInstanceOf[TypeSymbol]) => List((s.name.asInstanceOf[TypeSymbol], PathNode(p, k, s.name.asInstanceOf[TypeSymbol])))
          case s: TraitInfo => unpackSymbolPaths(syms, s).map( (sym,spath) => (sym,PathBranch(p, k, spath)) )
          case _ => List.empty[(TypeSymbol,SymPath)] // do nothing
        } ).toList
      case _ => List.empty[(TypeSymbol,SymPath)] // Only trait parents supported at this time.  Maybe TODO: non-case class parents
    }


trait SymPath:
  val parent: RType
  val parentSym: TypeSymbol
  def resolve(nav: TraitInfo): RType

case class PathNode( parent: RType, parentSym: TypeSymbol, childSym: TypeSymbol ) extends SymPath:
  def resolve(nav: TraitInfo): RType = nav.typedParams(parentSym)
  override def toString(): String = s"${parent.infoClass.getSimpleName}[$parentSym -> $childSym]"

case class PathBranch( parent: RType, parentSym: TypeSymbol, branch: SymPath ) extends SymPath:
  def resolve(nav: TraitInfo): RType = branch.resolve(nav.typedParams(parentSym).asInstanceOf[TraitInfo])
  override def toString(): String = s"${parent.infoClass.getSimpleName}[$parentSym -> ${branch.toString()}]"
