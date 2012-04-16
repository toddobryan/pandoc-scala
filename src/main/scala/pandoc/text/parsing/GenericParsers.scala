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
  
  def romanNumeral(isUpper: Boolean)(implicit state: ParserState): Parser[Int] = {
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
    def genericRoman(ten: String, five: String, one: String, mult: Int): Parser[Int] = {
      guard(regex("[%s]".format(romanDigits.mkString).r)) ~>
      ((regex("%s".format(one + ten).r) ^^^ 9 * mult)
      | (regex("%s".format(one + five).r) ^^^ 4 * mult)
      | ((regex("%s?".format(five).r) ^^ ((ds: String) => 5 * mult * ds.length))
          ~ (regex("%s{0,3}".format(one).r) ^^ ((cs: String) => 1 * mult * cs.length))).map {
            case ((fh: Int) ~ (oh: Int)) => fh + oh
          })
    }
    val hundreds: Parser[Int] = genericRoman(m, d, c, 100)
    val tens: Parser[Int] = genericRoman(c, l, x, 10)
    val ones: Parser[Int] = genericRoman(x, v, i, 1)
    (thousands ~ hundreds ~ tens ~ ones).map {
      case ((th: Int) ~ (hu: Int) ~ (te: Int) ~ (on: Int)) => th + hu + te + on
    }
  }
  
  def domain(implicit state: ParserState): Parser[String] = {
    val domainChar = """[a-zA-Z0-9\-]"""
    """%s+(\.%s+)+""".format(domainChar, domainChar).r
  }
  
  def emailAddress(implicit state: ParserState): Parser[(String, String)] = {
    ("""[a-zA-Z0-9][a-zA-Z0-9\-+_.]*@""".r ~ domain).map {
      // the Haskell version calls escapeURI on the mailto: string, but there can't be
      // spaces in it, so I don't think it's necessary
      case (addr ~ dom) => (addr + dom, ("mailto:" + addr + dom))
    }
  }
  
  def uri(implicit state: ParserState): Parser[(String, String)] = {
    //TODO isAllowedInURI
    val protocols = """(https?|ftp|file|mailto|news|telnet):""".r
    protocols.map((s: String) => (s, s))
  }
  
  def withHorizDisplacement[A](parser: Parser[A])(implicit state: ParserState): Parser[(A, Int)] = new Parser[(A, Int)] {
    def apply(in: Input): ParseResult[(A, Int)] = {
      val col1 = in.pos.column
      val res: ParseResult[A] = parser(in)
      val col2 = res.next.pos.column
      res.map((_, col2 - col1))
    }
  }
  
  def withRaw[A](parser: Parser[A])(implicit state: ParserState): Parser[(A, String)] = new Parser[(A, String)] {
    def apply(in: Input): ParseResult[(A, String)] = {
      println("in.source = " + in.source.toString)
      println("in.offset = " + in.offset)
      val res: ParseResult[A] = parser(in)
      println("res.next.source = " + res.next.source.toString)
      println("res.next.offset = " + res.next.offset)
      val offset2 = res.next.source.length - res.next.offset
      val offset1 = in.source.length - in.offset
      val raw = in.source.subSequence(in.offset, in.offset + (offset2 - offset1)).toString
      res.map((_, raw))
    }
  }
  
}