package pandoc.text.parsing
import scala.util.parsing.combinator.RegexParsers

object GenericParsers extends RegexParsers {
  // many1Till(p, end) => (p +) <~ end
  // notFollowedBy'(p) => not(p)
  
  override def skipWhitespace: Boolean = false
  
  def anyLine(implicit state: ParserState): Parser[String] = {
    """.*?\n""".r ^^ ((res: String) => res.dropRight(1))
  }

  def spaceChar(implicit state: ParserState): Parser[String] = """[ \t]""".r
  
  def nonspaceChar(implicit state: ParserState): Parser[String] = """[^\t\n \r]""".r
  
  def skipSpaces(implicit state: ParserState): Parser[String] = (spaceChar *) ^^^ ""
  
  def blankLine(implicit state: ParserState): Parser[String] = skipSpaces ~> "\n"
  
  def blankLines(implicit state: ParserState): Parser[String] = (blankLine +) ^^ ((ns: List[String]) => ns.mkString)
  
  def enclosed[A](start: Parser[Any], end: Parser[Any], content: Parser[A]): Parser[List[A]] = {
    start ~> not(" ") ~> (content +) <~ end
  }
  
  def stringAnyCase(str: String)(implicit state: ParserState): Parser[String] = {
    ("(?iu)" + str).r
  }
  
  def lineClump(implicit state: ParserState): Parser[String] = {
    blankLines |
    (((not(blankLine) ~> anyLine) +) ^^ ((lines: List[String]) => lines.mkString("\n")))
  }
  
  def charsInBalanced(open: Char, close: Char, content: Parser[String]): Parser[String] = {
    val openP: Parser[String] = "" + open
    val closeP: Parser[String] = "" + close
    def raw: Parser[List[String]] = {
      (((not(openP | closeP) ~> content) +) ^^ ((strs: List[String]) => strs.mkString) |
      (charsInBalanced(open, close, content) 
        ^^ ((str: String) => "%c%s%c".format(open, close, str)))) *
    }
    (openP ~> raw <~ closeP) ^^ ((strs: List[String]) => strs.mkString)
  }
  
  def romanNumeral(isUpper: Boolean)(implicit state: ParserState): Parser[Int] = new Parser[Int] {
    val lowerRomanDigits = List("i", "v", "x", "l", "c", "d", "m")
    val romanDigits: List[String] = {
      if (!isUpper) lowerRomanDigits else lowerRomanDigits.map(_.toUpperCase)
    }
    val i = romanDigits(0)
    val v = romanDigits(1)
    val x = romanDigits(2)
    val l = romanDigits(3)
    val c = romanDigits(4)
    val d = romanDigits(5)
    val m = romanDigits(6)
    val thousands: Parser[Int] = regex("%s{0,3}".format(m).r) ^^ ((ms: String) => 1000 * ms.length)
    val hundreds: Parser[Int] = ((regex("%s".format(c + m).r) ^^^ 900)
        | (regex("%s".format(c + d).r) ^^^ 400)
        | (regex("%s?".format(d).r) )
    
    
    val tens: Parser[Int] = {
      ((romanParsers(4) ~ romanParsers(2)) ^^^ 90) |
      ((romanParsers(4) ~ romanParsers()))
    }
    val patt: Parser[Int] = thousands ~ hundreds ~ tens ~ ones ^^ {
      case (Success(thou~hund~ten~one), _) => Failure("not a Roman numeral")
      case ()
    }
  }
}