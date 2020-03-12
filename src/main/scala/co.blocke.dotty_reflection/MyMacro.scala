package co.blocke.dotty_reflection

import scala.reflect.ClassTag

/**  This class is a "lab".  Not intended for use with the library.  It's a learning area to see if/how we might move
 *   pieces of this to a macro.
 */
import scala.quoted._

inline def assert(inline expr: Boolean): Unit = 
  ${ assertImpl('expr) }

def showExpr(expr: Expr[Boolean])(using QuoteContext): Expr[String] =
  '{ "<some source code>" } // Better implementation later in this document
  
def assertImpl(expr: Expr[Boolean])(using QuoteContext) = '{ 
  if (!$expr)
    // Note: Outer ${} is for string interpolation, not to be confused with evaluating an Expr!
    println(s"Error detected: ${${ showExpr(expr) }}") 
}

//------------------------------

inline def getPerson(inline expr: String) = 
  ${ getPersonObj('expr) }

def getPersonObj(expr: Expr[String])(using QuoteContext): Expr[Person] = 
  '{ Person($expr,45) }

 

//------------------------------  THERE'S HOPE!  This returns an Object, which is of the correct, created, type.

inline def getInstance[T](inline expr: String) = 
  ${ getInstanceObj('expr) }

def getInstanceObj(expr: Expr[String])(using QuoteContext): Expr[Object] = 
  '{ Class.forName($expr).getConstructors.head.newInstance().asInstanceOf[Object] }


  
//------------------------------  Shared cache

private val stuffCache = scala.collection.mutable.HashMap("a"->"co.blocke.dotty_reflection.Item")

inline def getInstance2[T](inline expr: String) = 
  ${ getInstanceObj2('expr) }

def getInstanceObj2(expr: Expr[String])(using QuoteContext): Expr[Object] = 
  '{ 
    val c = stuffCache($expr)
    stuffCache.put("b","co.blocke.dotty_reflection.Other")
    Class.forName(c).getConstructors.head.newInstance().asInstanceOf[Object]
   }


// <><><><><><><><>  First Attempt  <><><><><><><><>
  /*
inline def read[T](implicit ct: ClassTag[T]) = 
  ${ '{ ct.runtimeClass.getName } }

inline def read[T] = 
  // ${ getInstanceObj2('{"foo"}) }
  ${ readImpl       ('{     }) }
  //  ${ readImpl('{ct.asInstanceOf[Object]}) }

def readImpl[T](treeObj: Expr[Object])(implicit qctx: QuoteContext, wow:scala.quoted.Type[T]): Expr[Object] = 
  import qctx.tasty.{_, given _}
  println("HERE: " + treeObj.asInstanceOf[scala.quoted.Type[T]].unseal)
  '{ model.PrimitiveType.Scala_Int }
  */

// Macro entry point -- quoted w/o matching splice allowed
inline def read[T](x: => String): T = ${readImpl[T]('x)}

def readImpl[T](param: Expr[String])(implicit qctx: QuoteContext, wow:scala.quoted.Type[T]): Expr[T] = {
  import qctx.tasty.{_, given _}

  // YAY!!!  This knows about Boolean.  WE'RE SAVED!!!
  println("T: "+typeOf[T]) // AppliedType(TypeRef(ThisType(TypeRef(NoPrefix,module class dotty_reflection)),class Foo),List(TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),module scala),class Boolean)))

  // println("T: "+sym.asInstanceOf[dotty.tools.dotc.core.Symbols.Symbol].denot(qctx.tasty.rootContext.asInstanceOf[dotty.tools.dotc.core.Contexts.Context]))

  // println("T: "+sym.asInstanceOf[dotty.tools.dotc.core.Symbols.Symbol].asClass)
  //println("Sym: "+sym.getClass.getInterfaces.toList)

  // implicit val mutablePackagesMap: scala.collection.mutable.HashMap[String, EmulatedPackageRepresentation] = new scala.collection.mutable.HashMap[String, EmulatedPackageRepresentation]()
  // val classRef = new ClassRepresentation(qctx.tasty, t.classSymbol.get.asInstanceOf[ClassDef], None)
  // println("MAP: "+mutablePackagesMap)

  '{null.asInstanceOf[T]}
}
