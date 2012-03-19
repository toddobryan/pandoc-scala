package pandoc.text
import scala.util.parsing.combinator.RegexParsers

object Parsing extends RegexParsers {
  def emailChar: Parser[String] = regex("""[0-9a-zA-Z-+_.]"""r)
  //def emailAddress: Parser[(String, String)] = regex("""[0-9a-zA-Z]"""r) ~ emailChar * ~ literal("@")

}