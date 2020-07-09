package co.blocke.dotty_reflection
package impl

import info._
import extractors._
import scala.quoted._
import scala.reflect._
import scala.tasty.Reflection
import scala.tasty.inspector.TastyInspector
import dotty.tools.dotc.ast.Trees.AppliedTypeTree
  
class TastyInspection[T](clazz: Class[_], inTermsOf: TraitInfo) extends TastyInspector:

  var inspected: RType = UnknownInfo(clazz.getName)

  protected def processCompilationUnit(reflect: Reflection)(root: reflect.Tree): Unit = 
    import reflect.{_, given _}
    val tpe = Type(clazz)
    println("TPE: "+tpe)
    // tpe match {
    //   case a: AppliedType =>
    val args = inTermsOf.actualParameterTypes.map(p => Type(p.infoClass)).toList
    val a2 = AppliedType(tpe, args)
    println("A2: "+a2)
    inspected = RType.unwindType(reflect)(a2)
      // case a => 
      //   inspected = UnknownInfo("not parameterized")
    // }
    // println(">>> tpe: "+tpe)
    // implicit val ctx = inTermsOf.get.ctx
    // println("=== "+tpe.select(inTermsOf.get.traitType.get.classSymbol(ctx.get).asInstanceOf[reflect.Symbol]))
    // inspected = TastyReflection.reflectOn(reflect)( tpe )

/*
class TastyInspection(clazz: Class[_], inTermsOf: Option[TraitInfo], paramMap: TypeSymbolMap = Map.empty[TypeSymbol,RType]) 
    extends TastyInspector:
  import Clazzes._

  var inspected: RType = UnknownInfo(clazz.getName)
  val initialParamMap = inTermsOf match {
    case Some(ito) => ito.orderedTypeParameters.zip(ito.actualParameterTypes).toMap
    case None => paramMap
  }

  protected def processCompilationUnit(reflect: Reflection)(root: reflect.Tree): Unit = 
    import reflect.{_, given _}


    type PathSymbol = (String, TypeSymbol)
    val TypeSymPath = """(.*)\..+$""".r


    def unpackAppliedType( applied: AppliedType, lookForSyms: List[PathSymbol], typedTrait: TraitInfo ): Option[TypeSymbolMap] =
      val classSymbol = applied.tycon.asInstanceOf[reflect.TypeRef].classSymbol.get
      val AppliedType(classType, classTypeArgValues) = applied
      val classTypesParamList = classType.typeSymbol.primaryConstructor.paramSymss.head.map{ x =>
        val TypeSymPath(tsn) = x.fullName
        (tsn, x.name.asInstanceOf[TypeSymbol])
      }
      // Resolve the simple types 'T' first
      val symMap = classTypeArgValues.zipWithIndex.collect {
        case (tr: TypeRef,i) => 
          val TypeSymPath(tsn) = tr.typeSymbol.fullName
          ((tsn, tr.typeSymbol.name.asInstanceOf[TypeSymbol]), classTypesParamList(i))
      }.toMap
      val invertedSymMap = symMap.map(_.swap)
      val newLookForSyms = lookForSyms.map( s => symMap.getOrElse(s, s) )
      val simpleSymbols = findTypeSymValues(newLookForSyms, classSymbol, typedTrait,-1).map( typeSymMap => {
        // Now unwind any chained types
        val TypeSymPath(tsn) = classSymbol.fullName
        typeSymMap.map{ (k,v) => 
            val maybeSym = invertedSymMap.get( (classSymbol.fullName,k) )
            maybeSym match {
              case Some((_,s)) => s -> v
              case None => k -> v
            }
          }.toMap
      })
      
      // Now resolve any nested Traits (AppliedTypes)
      val nestedTraits = classTypeArgValues.zipWithIndex.findMap( a => a match {
        case (a: AppliedType, i: Int) => 
          // do stuff to stage call...
          val AppliedType(t2,tob2) = a
          val childClassSym = t2.classSymbol.get
          val childClassTypesParamList = t2.typeSymbol.primaryConstructor.paramSymss.head.map{ cls =>
            val TypeSymPath(tsn) = cls.fullName
            (tsn, cls.name.asInstanceOf[TypeSymbol])
          }
          val nestedSymMap = tob2.zipWithIndex.collect {
            case (tr: TypeRef,i) => 
              val TypeSymPath(tsn) = tr.typeSymbol.fullName
              ((tsn, tr.typeSymbol.name.asInstanceOf[TypeSymbol]), childClassTypesParamList(i))
          }.toMap
          val newLookForSyms2 = lookForSyms.map( s =>
            nestedSymMap.getOrElse(s, s)
            )
          val invertedSymMap = nestedSymMap.map(_.swap)
          val diveIntoTrait = typedTrait.actualParameterTypes(i).asInstanceOf[TraitInfo]
          unpackAppliedType(a, newLookForSyms2, diveIntoTrait).map( typeSymMap => {
            // Now unwind any chained types
            val TypeSymPath(tsn) = childClassSym.fullName
            typeSymMap.map{ (k,v) => 
                val maybeSym = invertedSymMap.get( (childClassSym.fullName,k) )
                maybeSym match {
                  case Some((_,s)) => s -> v
                  case None => k -> v
                }
              }.toMap
          })
        case _ => None
      })
      (nestedTraits, simpleSymbols) match {
        case (None,None) => None
        case (Some(n:TypeSymbolMap), None) => Some(n)
        case (None, Some(s:TypeSymbolMap)) => Some(s)
        case (Some(n:TypeSymbolMap), Some(s:TypeSymbolMap)) => Some( s ++ n )
      }


    def findTypeSymValues(
        lookForSyms: List[PathSymbol], 
        inClass: Symbol, 
        typedTrait: TraitInfo, 
        level: Int = 0
      ): Option[TypeSymbolMap] =
      
      // it's me
      if inClass.fullName == typedTrait.name then
        val myMap = typedTrait.orderedTypeParameters.zip( typedTrait.actualParameterTypes ).toMap
        Some(lookForSyms.collect{
            case s if myMap.contains(s._2) => (s._2 -> myMap(s._2))
          }.toMap)

      // it's somewhere in my parentage...
      else
        inClass.tree.asInstanceOf[ClassDef].parents.findMap( parent => { // return Option[TypeSymbolMap]
          parent match {
            case att: dotty.tools.dotc.ast.Trees.AppliedTypeTree[_] => unpackAppliedType( att.tpe.asInstanceOf[AppliedType], lookForSyms, typedTrait )
            case someTree: Tree if someTree.symbol.fullName == typedTrait.name => Some( lookForSyms.map(f => f._2 -> TypeSymbolInfo(f._2.asInstanceOf[String])).toMap )
            case _ => None
          }
        })

      
    reflect.rootContext match {
      case ctx if ctx.isJavaCompilationUnit() => inspected = JavaClassInspector.inspectJavaClass(clazz, initialParamMap)      
      case ctx if ctx.isScala2CompilationUnit() => inspected = Scala2Info(clazz.getName)  // Can't do much with Scala2 classes--not Tasty
      case _ => 
        val tpe = Type(clazz)
        val masterParamMap = 
          // If this is in terms of (some trait), map the class' unknown type symbol to the trait's known/typed param symbols.
          // For example:  
          //   trait Findable[F]
          //   case class Lost[L] extends Findable[L]
          // Upon call using Findable[Int], inTermsOf will be Some(TraitInfo(Findable, F->Int))
          // Now associate Lost.L -> Findable.F and finally L -> Int
          if inTermsOf.isDefined then   
            val classSymbol = tpe.classSymbol.get
            findTypeSymValues( getTypeParameters(reflect)(classSymbol).map(ts => (classSymbol.fullName,ts)), classSymbol, inTermsOf.get )
              .getOrElse(throw new ReflectException(s"${inTermsOf.get.name} is not a parent of ${clazz.getName}"))
          else 
            initialParamMap

        val tastyReflection: TastyReflection = TastyReflection(reflect,masterParamMap)( tpe )
        inspected = tastyReflection.reflectOn
    }
*/

/*

```scala
trait Movable[H,W]{ val h: H; val w: W }
case class Thing[A,B]( h: A, w: B ) extends Movable[A,B]
```

As you showed me last week, I can use Symbol.memberType to apply the types to the AppliedType so their
actual types are easy to get to.  Now I need to be able to "apply" the AppliedType from a Trait...
in other words: I want to see Thing *in terms of* Movable.

Something like this:

```scala
val myRtype = RType.of[Movable[Double,Long]]( "com.mystuff.Thing" )
```

I can reflect on the trait Movable at compile-time and know its parameters are Double and Long.  I only learn about
class Thing at runtime.  I can Tasty-inspect it at runtime and see that it's an AppliedType, that it has 2 parameters A and B,
and that it extends Movable.  

How can I see Thing *in terms of* Movable?
*/