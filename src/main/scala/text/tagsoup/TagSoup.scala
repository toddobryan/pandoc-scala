package text.tagsoup

case class Attr(name: String, value: String)
  
sealed abstract class Tag(val str: String)
case class TagOpen(s: String, attrs: List[Attr]) extends Tag(s)
case class TagClose(s: String) extends Tag(s)
case class TagText(s: String) extends Tag(s)
case class TagComment(s: String) extends Tag(s)
case class TagWarning(s: String) extends Tag(s)
case class TagPosition(row: Int, column: Int) extends Tag("")
