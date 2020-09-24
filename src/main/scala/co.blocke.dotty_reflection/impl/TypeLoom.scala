package co.blocke.dotty_reflection
package impl

import scala.tasty.Reflection
// import java.nio.ByteBuffer

// Ok, a no-no here... breaking out of the walls of Reflection and into compiler internals.
// Saves creationg of an Reflection context at runtime when one is strictly not needed.
// import dotty.tools.dotc.core.Types._ 

/**

Type-Weaving is the art of extracting and re-applying parameterized types to a generic class, in terms of a
reference--a Trait in our case.

The steps involved are:

1) Walk the class hierarchy (parentage) of a subject (given) class
2) For each AppliedType (parameterized class) parent do:
    - Figure out the type mappings of the subject class in terms of the parent
    - Repeat step 2 (get parents of this parent), but resolve the relative
       mappings: subject's param, in-terms-of parent0, in-terms-of parent1

trait Message[M,N]:
  val msg: M
  val n:   N

trait CommandMessage[C,D] extends Message[List[D],C]:
  val thing: C

case class Command[X,Y,Z](
  stuff: String, 
  one: X, 
  msg: List[Y], 
  n: List[Option[Z]],
  thing: Y) extends CommandMessage[Y, Option[Z]]

  Y and Z inTermsOf CommandMessage:
      MAPPING                DE-REF ACTION
      Y -> C                 C -> Y
      Y -> Param(0)

      Option[Z] -> D         un-apply[D] -> Z
      Option[Z] -> Param(1)  un-apply[Param(1)] -> Z

  Y and Z inTermsOf Message:  
      MAPPING                        DE-REF ACTION
      Y -> N                         N -> Y
      Y -> Param(1)                  Param(1) -> Y

      List[Option[Z]] -> M           un-apply[un-apply[M]] -> Z
      List[Option[Z]] -> Param(0)    un-apply[aun-apply[Param(0)]] -> Z
 */

object TypeLoom:


/*
  def extractSymbolTypes(reflect: Reflection)( 
      subject: reflect.Type, 
      symbols: Set[reflect.Symbol], 
      foundSoFar: Map[String, reflect.Type] = Map.empty[String, reflect.Type] 
    ): Map[String, reflect.Type] =
    import reflect._ 

    subject match {
      case AppliedType(t,tob) =>
        tob.foldLeft( (symbols,foundSoFar) ){ case((symsToFind, soFar),tpe) =>
          val gotSome = extractSymbolTypes(reflect)(tpe, symsToFind, soFar)
          (symsToFind.filterNot(s => gotSome.keySet.contains(s.fullName.replace("_$",""))), gotSome)
          }._2
      
      case _ => // found a symbol
        subject.typeSymbol match {
          case ts if ts.isTypeDef && symbols.contains(ts) =>
            foundSoFar + (subject.typeSymbol.fullName.replace("_$","") -> subject)

          case _ =>  // a class or trait of some sort
            subject.classSymbol.get match {
              case sym if sym.isClassDef =>
                if sym.tree.asInstanceOf[ClassDef].constructor.paramss.head.nonEmpty then
                  sym.tree.asInstanceOf[ClassDef].constructor.paramss.head.foldLeft( (symbols,foundSoFar) ){ case ((symsToFind, soFar),fieldValDef) => 
                    val gotSome = extractSymbolTypes(reflect)(fieldValDef.tpt.tpe, symsToFind, soFar) 
                    (symsToFind.filterNot(s => gotSome.keySet.contains(s.fullName.replace("_$",""))), gotSome)
                    }._2
                else
                  // no params on this class -- pass through
                  foundSoFar
            }
        }
  }
  */


  /*
  def descendParents( subject: Type, lookFor: Set[TypeSymbol] ): Map[String, Map[String, List[PathNode]]] =
    val classDef = subject.classSymbol.get.tree.asInstanceOf[ClassDef]
    val lookForFullPath = lookFor.map( s => subject.classSymbol.get.fullName + "." + s.asInstanceOf[String] )
    classDef.parents.collect{
      case t: TypeTree => t.tpe
    }.collect{
      case a: AppliedType => 
        (a.typeSymbol.fullName -> descendAppliedType(reflect)(a, Nil, Map.empty[String, List[PathNode]], lookForFullPath))
    }.toMap


  def descendAppliedType( applied: AppliedType, pathSoFar: List[PathNode], foundSoFar: Map[String, List[PathNode]], lookFor: Set[String] ): Map[String, List[PathNode]] =
    applied.args.zipWithIndex.foldLeft( (lookFor,foundSoFar) ){ case( (look,fsf), (oneT,i) ) =>
      oneT match {
        case one if look.contains(one.typeSymbol.fullName) => 
          (look - one.typeSymbol.fullName, fsf + (one.typeSymbol.fullName -> (pathSoFar :+ PathNodeChoose(i))))
        case a: AppliedType => 
          val psf = pathSoFar ++ List(PathNodeChoose(i), PathNodeApplied())
          val found = descendAppliedType(a, psf, foundSoFar, lookFor)
          (look -- found.keySet, found)
        case _ =>
          (look, foundSoFar) // do nothing
      }
    }._2
    */


  def descendParents(reflect: Reflection)( subject: reflect.Type, lookFor: Set[TypeSymbol] ): Map[String, Map[String, List[PathNode]]] =
    import reflect._
    val classDef = subject.classSymbol.get.tree.asInstanceOf[ClassDef]
    val lookForFullPath = lookFor.map( s => subject.classSymbol.get.fullName + "." + s.asInstanceOf[String] )
    classDef.parents.collect{
      case t:reflect.TypeTree => t.tpe
    }.collect{
      case a: AppliedType => 
        (a.typeSymbol.fullName -> descendAppliedType(reflect)(a, Nil, Map.empty[String, List[PathNode]], lookForFullPath))
    }.toMap


  def descendAppliedType(reflect: Reflection)( applied: reflect.AppliedType, pathSoFar: List[PathNode], foundSoFar: Map[String, List[PathNode]], lookFor: Set[String] ): Map[String, List[PathNode]] =
    import reflect._
    val AppliedType(t,tob) = applied
    tob.zipWithIndex.foldLeft( (lookFor,foundSoFar) ){ case( (look,fsf), (oneT,i) ) =>
      oneT match {
        case one if look.contains(one.typeSymbol.fullName) => 
          (look - one.typeSymbol.fullName, fsf + (one.typeSymbol.fullName -> (pathSoFar :+ PathNodeChoose(i))))
        case a: AppliedType => 
          val psf = pathSoFar :+ PathNodeChoose(i)
          val found = descendAppliedType(reflect)(a, psf, foundSoFar, lookFor)
          (look -- found.keySet, found)
        case _ =>
          (look, foundSoFar) // do nothing
      }
    }._2

  
  trait PathNode
  case class PathNodeApplied() extends PathNode  // idx is which tob
  case class PathNodeChoose(idx: Int) extends PathNode   // idx is which param in the list

  object Recipe:
    def navigate( m: Map[String, List[PathNode]], rtype: RType ): Map[TypeSymbol, RType] =
      m.map {
        case (sym, path) =>
          val fixedSym = sym.drop(sym.lastIndexOf('.')+1)
          var rt = rtype
          path.map{ _ match {
            case PathNodeChoose(i) => 
              rt = rt.asInstanceOf[AppliedRType].select(i)
          }}
          ( fixedSym.asInstanceOf[TypeSymbol], rt )
      }.toMap

    // tob.zipWithIndex.collect {
    //   case (one,i) if lookFor.contains(one.typeSymbol.fullName) => 
    //     foundSoFar + (one.typeSymbol.fullName -> (pathSoFar :+ PathNodeChoose(i)))
    //   case (a: AppliedType, i) => 
    //     val psf = pathSoFar ++ List(PathNodeChoose(i), PathNodeApplied())
    //     descendAppliedType(reflect)(a, psf, foundSoFar, lookFor)
    //   case _ =>
    //     foundSoFar // do nothing
    // }


    /*
  inline def addPathNode( m: Map[String, List[PathNode]], sym: String, node: PathNode ): Map[String, List[PathNode]] =
    if m.contains(sym) then
      m + (sym -> (m(sym) :+ node))
    else
      m + (sym -> List(node))

  def findSymbolsInParents(reflect: Reflection)( subject: reflect.Type, symTypeMap: Map[String,reflect.Type] ): Unit = //Map[String, Recipe]
    import reflect._
    val classDef = subject.classSymbol.get.tree.asInstanceOf[ClassDef]
    classDef.parents.collect{
      case t:reflect.TypeTree => t.tpe
    }.collect{
      case AppliedType(t,tob) => 
        println("Find: "+symTypeMap.keySet)
        println("Dad: "+ t.typeSymbol.fullName +" >> "+tob.map(_.typeSymbol))
        val r = Recipe(t.typeSymbol.fullName)
        val finalRecipe = tob.zipWithIndex.collect {
          case (one,i) if symTypeMap.contains(one.typeSymbol.fullName) => r.add(one.typeSymbol.fullName, PathNodeSymbol(i))
          case (AppliedType(t2,tob2),i) => 
            r.add(one.typeSymbol.fullName, PathNodeSymbol(i))
        }
        println("Final: "+finalRecipe)
        println("-------\n")
      }
      */


  // case class Recipe(forClass: String, nodesForSymbol: Map[String,List[PathNode]] = Map.empty[String,List[PathNode]]):
  //   def add(sym: String, r:PathNode): Recipe = 
  //     val newNodes = this.nodesForSymbol.get(sym).map(_ :+ r).getOrElse(List(r))
  //     this.copy(nodesForSymbol = this.nodesForSymbol + (sym -> newNodes))

  // Foo[String,C] -> PathNodeSymbol(1)
  // Foo[String,Option[C]] --> [PathNodeSymbol(1), PathNodeApplied, RepeNodeSymbol(0)]

  /*
  val RECIPE_APPLIED = 1
  val RECIPE_SYMBOL  = 2

  case class Recipe protected ( buf: ByteBuffer ):
    def add( element: Byte, arg: Byte = -1 ): Recipe =  // WARNING: Mutable state!  Not functional...
      buf.put(element)
      if arg >= 0 then 
        buf.put(arg)
      this

    // Fork for: Map K/V, Tuple fields, class fields, trait fields
    def fork: Recipe =
      val buf2 = ByteBuffer.wrap(buf.array)
      Recipe( buf2.position(buf.position()) )

    // Call this to "freeze" (i.e. hard-copy) the path (underlying array)
    // PROBLEM: May be locked or unlocked when toString is called... affects buf.position().  If locked, position() is 0
    def lock: Recipe = 
      val newBuf = ByteBuffer.wrap(buf.array.slice(0, buf.position()))
      newBuf.position(buf.position())
      Recipe(newBuf)
*/