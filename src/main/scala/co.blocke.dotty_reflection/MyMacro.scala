package co.blocke.dotty_reflection

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

  