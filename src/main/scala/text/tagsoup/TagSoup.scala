package text.tagsoup

case class Attr(name: String, value: String)
  
sealed abstract class Tag(val str: String)
case class TagOpen(str: String, attrs: List[Attr]) extends Tag(str)
case class TagClose(str: String) extends Tag(str)
case class TagText(str: String) extends Tag(str)
case class TagComment(str: String) extends Tag(str)
case class TagWarning(str: String) extends Tag(str)
case class TagPosition(row: Int, column: Int) extends Tag("")
