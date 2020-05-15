package co.blocke.dotty_reflection
package info

/** Something to smooth the differences between the 2.x Enumeration class and the 3.x Enum class
 */
trait EnumInfo extends RType:
  val infoClass: Class[_]
  lazy val values: List[Any]
  def ordinal(s: String): Int
  def valueOf(s: String): Any
  def valueOf(i: Int): Any
  def show(tab: Int = 0, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName + s" with values [${values.map(_.toString).mkString(",")}]\n"


case class ScalaEnumInfo protected[dotty_reflection](
  name: String,
  infoClass: Class[_]
) extends EnumInfo: 
  val orderedTypeParameters = Nil

  private val companion = Class.forName(name+"$")
  private val companionConst = companion.getDeclaredConstructor()
  companionConst.setAccessible(true)
  private val valuesMethod = companion.getMethod("values")
  private val ordinalMethod = companion.getMethod("ordinal", classOf[Object])
  private val companionInstance = companionConst.newInstance()
  private val valueOfMethod = companion.getMethod("valueOf", classOf[String])

  lazy val values: List[Any] = valuesMethod.invoke(companionInstance).asInstanceOf[Array[_]].toList
  def ordinal(s: String): Int = 
    val target = valueOfMethod.invoke(companionInstance, s)
    ordinalMethod.invoke(companionInstance, target).asInstanceOf[Int]
  def valueOf(s: String): Any = valueOfMethod.invoke(companionInstance,s)
  def valueOf(i: Int): Any = values(i)


case class ScalaEnumerationInfo protected[dotty_reflection](
  name: String,
  infoClass: Class[_]
) extends EnumInfo:
  val orderedTypeParameters = Nil

  private val companion = Class.forName(name+"$")
  private val companionConst = companion.getDeclaredConstructor()
  companionConst.setAccessible(true)
  private val valuesMethod = companion.getMethod("values")
  private val companionInstance = companionConst.newInstance()
  private val withNameMethod = companion.getMethod("withName", classOf[String])
  private val applyMethod = companion.getMethod("apply", classOf[Int])

  lazy val values: List[Any] = valuesMethod.invoke(companionInstance).asInstanceOf[Set[_]].toList
  def ordinal(s: String): Int = valueOf(s).asInstanceOf[Enumeration#Value].id
  def valueOf(s: String): Any = withNameMethod.invoke(companionInstance,s)
  def valueOf(i: Int): Any = applyMethod.invoke(companionInstance,i.asInstanceOf[Object])


  
case class JavaEnumInfo protected[dotty_reflection](
  name: String,
  infoClass: Class[_]
) extends RType: 
  val orderedTypeParameters = Nil

  def show(tab: Int = 0, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName +s"(${infoClass.getName})\n"
