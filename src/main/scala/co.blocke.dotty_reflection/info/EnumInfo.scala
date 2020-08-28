package co.blocke.dotty_reflection
package info

/** Something to smooth the differences between the 2.x Enumeration class and the 3.x Enum class
 */
trait EnumInfo extends Transporter.RType:
  lazy val infoClass: Class[_]
  val values: List[String]
  def ordinal(s: String): Int
  def valueOf(s: String): Any
  def valueOf(i: Int): Any
  def show(tab: Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName + s"($name) with values [${values.map(_.toString).mkString(",")}]\n"


case class ScalaEnumInfo protected[dotty_reflection](
  name: String,
  values: List[String]
) extends EnumInfo: 
  val fullName = name
  lazy val infoClass: Class[_] = Class.forName(name)

  private lazy val ordinalMethod = infoClass.getMethod("ordinal")
  private lazy val valuesMethod  = infoClass.getMethod("values")
  private lazy val valueOfMethod = infoClass.getMethod("valueOf", classOf[String])

  def ordinal(s: String): Int = ordinalMethod.invoke(valueOfMethod.invoke(null, s)).asInstanceOf[Int]
  def valueOf(s: String): Any = valueOfMethod.invoke(null,s)
  def valueOf(i: Int): Any = valuesMethod.invoke(null).asInstanceOf[Array[Object]].find(e => ordinalMethod.invoke(e).asInstanceOf[Int] == i).get


case class ScalaEnumerationInfo protected[dotty_reflection](
  name: String,
  values: List[String]
) extends EnumInfo:
  val fullName = name
  lazy val infoClass: Class[_] = Class.forName(name)

  private lazy val withNameMethod = infoClass.getMethod("withName", classOf[String])
  private lazy val applyMethod = infoClass.getMethod("apply", classOf[Int])

  def ordinal(s: String): Int = valueOf(s).asInstanceOf[Enumeration#Value].id
  def valueOf(s: String): Any = withNameMethod.invoke(null,s)
  def valueOf(i: Int): Any = applyMethod.invoke(null,i.asInstanceOf[Object])


case class JavaEnumInfo protected[dotty_reflection](
  name: String,
) extends Transporter.RType: 
  val fullName = name
  lazy val infoClass: Class[_] = Class.forName(name)

  def show(tab: Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName +s"(${infoClass.getName})\n"
