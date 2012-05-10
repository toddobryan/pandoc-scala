package pandoc.text

import java.net.{URI, URISyntaxException, URLEncoder}

import scala.math.max
import scala.util.parsing.input.CharSequenceReader
import scala.util.parsing.input.Reader

import Definition._
import Shared._

case class SourcePos(sourceName: String, line: Int, column: Int)

sealed abstract class HeaderType
case class SingleHeader(ch: Char) extends HeaderType
case class DoubleHeader(ch: Char) extends HeaderType

sealed abstract class ParserContext
case object ListItemState extends ParserContext
case object NullState extends ParserContext

case class Key(content: List[Inline])

sealed abstract class QuoteContext
case object InSingleQuote extends QuoteContext
case object InDoubleQuote extends QuoteContext
case object NoQuote extends QuoteContext

case class Context(raw: Boolean = false, list: ParserContext = NullState, 
    quote: QuoteContext = NoQuote, maxNestingLevel: Int = 6)

case class ParserState(
  context: Context = Context(),
  lastStrPos: Option[SourcePos] = None,
  keys: Map[Key, Target] = Map(),
  citations: List[String] = Nil,
  notes: List[(String, String)] = Nil,
  tabStop: Int = 4,
  standalone: Boolean = false,
  title: List[Inline] = Nil,
  authors: List[List[Inline]] = Nil,
  date: List[Inline] = Nil,
  strict: Boolean = false,
  smart: Boolean = false,
  oldDashes: Boolean = false,
  literateHaskell: Boolean = false,
  columns: Int = 80,
  headerTable: List[HeaderType] = Nil,
  indentedCodeClasses: List[String] = Nil,
  nextExample: Int = 1,
  examples: Map[String, Int] = Map(),
  hasChapters: Boolean = false,
  applyMacros: Boolean = false, // TODO: macros not supported yet, because that's a whole nother lib
  macros: List[String] = Nil    // TODO: should be List[Macro]
)

trait Parsing extends StatefulParsers[ParserState] {
  def parse[T](parser: Parser[T], in: String): ParseResult[T] = {
    parser(new StatefulReader[ParserState, Elem](ParserState(), new CharSequenceReader(in)))
  }
  
  implicit def listOfCharToString(lcParser: Parser[List[Char]]): Parser[String] = {
    lcParser ^^ ((cs: List[Char]) => cs.mkString)
  }
  
  def anyLine: Parser[String] = """[^\n]*\n""".r ^^ (_.dropRight(1))
  def spaceChar: Parser[Char] = elem(' ') | elem('\t')
  def nonspaceChar: Parser[Char] = elem("non-space character", (c: Char) => !"\t\n \r".contains(c))
  def skipSpaces: Parser[Unit] = spaceChar.* ^^^ ()
  def blankLine: Parser[String] = skipSpaces ~> "\n"
  def blankLines: Parser[String] = blankLine.+ ^^ ((nls: List[String]) => nls.mkString)
  
  def enclosed[T](start: Parser[Any], end: Parser[Any], parser: Parser[T]): Parser[List[T]] = {
    start ~> guard(not(" ")) ~> parser.+ <~ end
  }
  
  def stringAnyCase(s: String): Parser[String] = {
    def helper(cs: List[Char]): Parser[List[Char]] = cs match {
      case Nil => "" ^^^ Nil
      case x :: xs => for {
        firstChar <- elem(x.toUpper) | elem(x.toLower)
        rest <- helper(xs)
      } yield firstChar :: rest
    }
    helper(s.toList)
  }
  
  def parseFromString[A](parser: Parser[A], str: String): Parser[A] = {
    for {
      // don't need to deal with position as the Haskell code does
      // because the position is stored in the Reader
      oldInput <- getInput
      _ <- setInput(new CharSequenceReader(str))
      result <- parser
      _ <- setInput(oldInput)
    } yield result
  }
  
  def lineClump: Parser[String] = {
    blankLines | 
    ((not(blankLine) ~> anyLine).+ ^^ ((lines: List[String]) => lines.mkString("\n")))
  }
  
  def charsInBalanced(open: Char, close: Char, parser: Parser[Char]): Parser[String] = {
    def notOpenOrClose: Parser[Unit] = not(elem(open) | elem(close))
    def nested: Parser[String] = {
      listOfCharToString((notOpenOrClose ~> parser).+) |
      (for {
        res <- charsInBalanced(open, close, parser)
      } yield "%c%s%c".format(open, res, close))
    }
    for {
      _ <- elem(open)
      raw <- nested.*
      _ <- elem(close)
    } yield raw.mkString
  }
  
  def romanNumeral(isUpper: Boolean): Parser[Int] = {
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
  
  def emailChar: Parser[Char] = {
    elem("valid character in email addresses",
         (c: Char) => c.isLetterOrDigit || "-+_.".contains(c))
  }
  
  def domainChar: Parser[Char] = {
    elem("valid character in a domain", ((c: Char) => c.isLetterOrDigit || c == '-'))
  }
  
  def domain: Parser[String] = for {
    first <- domainChar.+ ^^ ((cs: List[Char]) => cs.mkString)
    dom <- (elem('.') ~> listOfCharToString(domainChar.+)).+
  } yield (first :: dom).mkString(".")

  def emailAddress: Parser[(String, String)] = for {
    firstLetter <- elem("alpha-numeric", _.isLetterOrDigit)
    restAddr: String <- listOfCharToString(emailChar.*)
    addr: String = firstLetter + restAddr
    _ <- elem('@')
    dom <- domain
    full = addr + "@" + dom
  } yield (full, escapeUri("mailto:" + full))

  def isPunctuation(c: Char): Boolean = {
    val punctClasses = List(Character.CONNECTOR_PUNCTUATION,
                            Character.DASH_PUNCTUATION,
                            Character.START_PUNCTUATION,
                            Character.END_PUNCTUATION,
                            Character.INITIAL_QUOTE_PUNCTUATION,
                            Character.FINAL_QUOTE_PUNCTUATION,
                            Character.OTHER_PUNCTUATION)
    punctClasses.contains(c.getType)
  }
  
  def isAscii(c: Char) = c.toInt < 128
  def isAllowedInUri(c: Char) = isReserved(c) || isUnreserved(c) || c == '%'
  def isReserved(c: Char) = isGenDelims(c) || isSubDelims(c)
  def isGenDelims(c: Char) = ":/?#[]@".contains(c)
  def isSubDelims(c: Char) = "!$&'()*+,;=".contains(c)
  def isUnreserved(c: Char) = isAlphaNumChar(c) || "-_.~".contains(c)
  def isAlphaNumChar(c: Char) = isAlphaChar(c) || isDigitChar(c)
  def isAlphaChar(c: Char) = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
  def isDigitChar(c: Char) = c >= '0' && c <= '9'
  def parseUri(str: String): Option[URI] = try {
    Some(new URI(str))
  } catch {
    case e: URISyntaxException => None
  }
  
  def uri: Parser[(String, String)] = {
    val protocols = List("http", "https", "ftp", "mailto", "news", "telnet")
    val protocolPatt = """(%s):""".format(protocols.mkString("|")).r
    val innerPunct = {
      elem("punctuation", ((c: Char) => isPunctuation(c))) <~ 
      not(elem('\n') | spaceChar)
    }
    val uriChar = innerPunct | 
                  elem("URI character",
                       (c: Char) => !isPunctuation(c) && (!isAscii(c) ||  isAllowedInUri(c)))
    val inParens = (elem('(') ~> listOfCharToString(uriChar.*) <~ elem(')')) ^^ 
                     ((s: String) => "(" + s + ")")
    def parser: Parser[String] = for {
      _ <- guard(protocolPatt)
      str <- ((inParens | ((innerPunct | uriChar) ^^ ((c: Char) => c.toString))).+) ^^ 
             ((strs: List[String]) => strs.mkString)
    } yield str
    StatefulParser[(String, String)] { (in: StatefulReader[ParserState, Elem]) =>
      parser(in).map((str: String) => (str, parseUri(escapeUri(str)))).
        mapPartial({
          case (str, Some(uri)) if (protocols.contains(uri.getScheme)) => (str, uri.toASCIIString)
        }, (arg: (String, Option[URI])) => "not a URI")  
    }
  }
  
  def withHorizDisplacement[A](parser: => Parser[A]): Parser[(A, Int)] = {
    for {
      inp1: Reader[Elem] <- getInput
      result: A <- parser
      inp2: Reader[Elem] <- getInput
    } yield (result, inp2.pos.column - inp1.pos.column)
  }
  
  private def readLines(src: String, num: Int): List[String] = {
    if (num == 0) Nil
    else {
      val nextNewline = math.max(src.indexOf("\n"), src.length - 1)
      src.substring(0, nextNewline) :: readLines(src.substring(nextNewline + 1), num - 1)
    }
  }
  
  def withRaw[A](parser: => Parser[A]): Parser[(A, String)] = {
    for {
      inp1 <- getInput
      result <- parser
      inp2 <- getInput
      (l1, c1) = (inp1.pos.line, inp1.pos.column)
      (l2, c2) = (inp2.pos.line, inp2.pos.column)
      inpLines = readLines(inp1.source.toString, (l2 - l1) + 1)
      raw = inpLines match {
        case Nil => throw new RuntimeException("raw: inpLines is Nil")
        case l :: Nil => l.substring(0, c2 - c1)
        case ls => ls.init.mkString("\n") + ls.last.substring(0, c2 - 1)
      }
    } yield (result, raw)
  }
  
  def nullBlock: Parser[Block] = """.""".r ^^^ EmptyBlock
  
  def failIfStrict: Parser[Unit] = StatefulParser[Unit] { (in: StatefulReader[ParserState, Elem]) =>
    if (!in.state.strict) success(())(in)
    else failure("strict mode")(in)
  }
  
  def failUnlessLhs: Parser[Unit] = StatefulParser[Unit] { (in: StatefulReader[ParserState, Elem]) =>
    if (in.state.literateHaskell) success(())(in)
    else failure("not literate Haskell")(in)
  }
  
  def escaped(parser: Parser[Char]): Parser[Char] = elem('\\') ~> parser
  
  def characterReference: Parser[Char] = StatefulParser[Char] { 
    (in: StatefulReader[ParserState, Elem]) => {
      (elem('&') ~> """[^ \t\n\r;]+""".r <~ elem(';'))(in).
      map(entityMap.get(_)).mapPartial((oc: Option[Char]) => oc match {
        case Some(c) => c
      }, (oc: Option[Char]) => "entity not found")
    }
  }

  def upperRoman: Parser[(ListNumberStyle, Int)] = for {
    num <- romanNumeral(true)
  } yield (UpperRoman, num)
  
  def lowerRoman: Parser[(ListNumberStyle, Int)] = for {
    num <- romanNumeral(false)
  } yield (LowerRoman, num)
  
  def decimal: Parser[(ListNumberStyle, Int)] = """\d+""".r ^^ ((num: String) => (Decimal, num.toInt))
  
  def exampleNum: Parser[(ListNumberStyle, Int)] = for {
    _ <- elem('@')
    lab: String <- listOfCharToString(elem("label characters", ((c: Char) => c.isLetterOrDigit || c == '_' || c == '-')).*)
    st <- getState
    num = st.nextExample
    newLabels = if (lab == "") st.examples
                else st.examples + (lab -> num)
    _ <- setState(st.copy(nextExample = num + 1, examples = newLabels))
  } yield (Example, num)
  
  def defaultNum: Parser[(ListNumberStyle, Int)] = elem('#') ^^^ (DefaultStyle, 1)
  
  def lowerAlpha: Parser[(ListNumberStyle, Int)] = {
    elem("lower alpha", ((c:Char) => c >= 'a' && c <= 'z')) ^^ 
      ((c: Char) => (LowerAlpha, (c.toInt - 'a'.toInt) + 1))
  }
  
  def upperAlpha: Parser[(ListNumberStyle, Int)] = {
    elem("upper alpha", ((c: Char) => c >= 'A' && c <= 'Z')) ^^
      ((c: Char) => (UpperAlpha, (c.toInt - 'A'.toInt) + 1))
  }
  
  def romanOne: Parser[(ListNumberStyle, Int)] = {
    elem('i') ^^^ (LowerRoman, 1) | elem('I') ^^^ (UpperRoman, 1)
  }
  
  def anyOrderedListMarker: Parser[ListAttributes] = {
    val listAttribParsers: List[Parser[ListAttributes]] = for {
      delimParser <- List(inPeriod _, inOneParen _, inTwoParens _)
      numParser <- List(decimal, exampleNum, defaultNum, romanOne,
                        lowerAlpha, lowerRoman, upperAlpha, upperRoman)
    } yield delimParser(numParser)
    choice(listAttribParsers.head, (listAttribParsers.tail): _*)
  }
  
  def inPeriod(num: Parser[(ListNumberStyle, Int)]): Parser[ListAttributes] = {
    for {
      styleAndStart <- num
      (style, start) = styleAndStart
      _ <- elem('.')
      delim = if (style == DefaultStyle) DefaultDelim else Period
    } yield ListAttributes(start, style, delim)
  }

  def inOneParen(num: Parser[(ListNumberStyle, Int)]): Parser[ListAttributes] = {
    for {
      styleAndStart <- num
      (style, start) = styleAndStart
      _ <- elem(')')
    } yield ListAttributes(start, style, OneParen)
  }
  
  def inTwoParens(num: Parser[(ListNumberStyle, Int)]): Parser[ListAttributes] = {
    for {
      _ <- elem('(')
      styleAndStart <- num
      (style, start) = styleAndStart
      _ <- elem(')')
    } yield ListAttributes(start, style, TwoParens)
  }
  
  def orderedListMarker(style: ListNumberStyle, delim: ListNumberDelim): Parser[Int] = {
    val num = defaultNum | (style match {
      case DefaultStyle => decimal
      case Example => exampleNum
      case Decimal => decimal
      case UpperRoman => upperRoman
      case LowerRoman => lowerRoman
      case UpperAlpha => upperAlpha
      case LowerAlpha => lowerAlpha
    })
    val context = delim match {
      case DefaultDelim => inPeriod _
      case Period => inPeriod _
      case OneParen => inOneParen _
      case TwoParens => inTwoParens _
    }
    for {
      startStyleAndDelim <- context(num)
    } yield startStyleAndDelim.num
  }
  
  def charRef: Parser[Inline] = {
    for {
      c <- characterReference
    } yield Str(c.toString)
  }
  
  def tableWith[Sep, End](headerParser: Parser[(List[TableCell], List[Alignment], List[Int])],
                rowParser: (List[Int] => Parser[List[TableCell]]),
                lineParser: Parser[Sep],
                footerParser: Parser[End],
                captionParser: Parser[List[Inline]]): Parser[Block] = {
    val optCaption = captionParser.? ^^ ((cp: Option[List[Inline]]) => cp match {
      case Some(inlines) => inlines
      case None => Nil
    })
    for {
      captionPrime <- optCaption
      headsAlignsAndIndices <- headerParser
      (heads, aligns, indices) = headsAlignsAndIndices
      linesPrime <- repsep(rowParser(indices), lineParser) <~ lineParser.?
      _ <- footerParser
      caption <- if (captionPrime.isEmpty) optCaption else success(captionPrime)
      state <- getState
      numColumns = state.columns
      widths = widthsFromIndices(numColumns, indices)
    } yield Table(caption, aligns, widths, heads, linesPrime)
  }
  
  def widthsFromIndices(numColumnsPrime: Int, indices: List[Int]): List[Double] = {
    if (indices.isEmpty) Nil
    else {
      val numColumns = max(numColumnsPrime, if (indices.isEmpty) 0 else indices.last)
      val lengthsPrime = indices.zip(0 :: indices).map {
        case (i1, i2) => i1 - i2
      }
      val lengths = (lengthsPrime.reverse match {
        case Nil => Nil
        case x :: Nil => List(x)
        case x :: y :: zs => if (x < y && y - x <= 2) y :: y :: zs else x :: y :: zs
      }).reverse
      val totLength = lengths.sum
      val quotient = max(totLength, numColumns) * 1.0
      val fracs = lengths.map((l: Int) => l / quotient)
      fracs.tail
    }
  }
  
  def gridTableWith(block: Parser[Block],
                    tableCaption: Parser[List[Inline]], 
                    isHeadless: Boolean): Parser[Block] = {
    tableWith(gridTableHeader(isHeadless, block), gridTableRow(block), gridTableSep('-'),
              gridTableFooter, tableCaption)
  }
  
  def gridTableSplitLine(indices: List[Int], line: String): List[String] = {
    splitStringByIndices(indices.init, removeTrailingSpace(line)).tail.map(removeFinalBar(_))
  }
  
  def gridPart(c: Char): Parser[(Int, Int)] = {
    for {
      dashes <- elem(c).+
      _ <- elem('+')
    } yield (dashes.length, dashes.length + 1)
  }
  
  def gridDashedLines(c: Char): Parser[List[(Int, Int)]] = {
    elem('+') ~> gridPart(c).+ <~ blankLine
  }
  
  def removeFinalBar(s: String): String = {
    s.reverse.dropWhile(_ == '|').dropWhile(" \t".contains(_)).reverse
  }
  
  def gridTableSep(c: Char): Parser[Char] = {
    gridDashedLines(c) ^^^ '\n'
  }
  
  private def mapM[A, B](f: A => Parser[B], as: Seq[A]): Parser[List[B]] = {
    as match {
      case Nil => success(Nil)
      case first :: rest => for {
        b <- f(first)
        bs <- mapM(f, rest)
      } yield b :: bs
    }
  }
  
  private def liftM[A, B](f: A => B, pa: Parser[A]): Parser[B] = {
    for {
      a <- pa
    } yield f(a)
  }

  def gridTableHeader(isHeadless: Boolean, block: Parser[Block]): Parser[(List[TableCell], List[Alignment], List[Int])] = {
    for {
      _ <- blankLines.?
      dashes <- gridDashedLines('-')
      rawContent <- if (isHeadless) success(()) ^^^ List.fill(1000)("")
                    else (not(gridTableSep('=')) ~> elem('|') ~> ".+".r <~ elem('\n')).+
      _ <- if (isHeadless) success(()) else gridTableSep('=') ^^^ ()
      linesPrime = dashes.map(_._2)
      indices = linesPrime.scanLeft(0)((x: Int, y: Int) => x + y)
      aligns = List.fill(linesPrime.length)(AlignDefault)
      rawHeads = if (isHeadless) List.fill(dashes.length)("")
                 else rawContent.map(gridTableSplitLine(indices, _)).transpose.map(_.mkString(" "))
      heads <- mapM((s: String) => parseFromString(block.*, s), rawHeads.map(removeLeadingTrailingSpace(_)))
    } yield (heads.map(TableCell(_)), aligns, indices)
  }
  
  def gridTableRawLine(indices: List[Int]): Parser[List[String]] = {
    for {
      _ <- elem('|')
      line <- ".+".r <~ elem('\n')
    } yield gridTableSplitLine(indices, line)
  }

  
  def gridTableRow(block: Parser[Block])(indices: List[Int]): Parser[List[TableCell]] = {
    for {
      colLines <- gridTableRawLine(indices).*
      cols = colLines.transpose.map((strs: List[String]) => removeOneLeadingSpace(strs).mkString("\n") + "\n")
      res <- mapM((str: String) => parseFromString(block.*, str) ^^ ((bs: List[Block]) => compactifyCell(bs)), cols)
    } yield res.map(TableCell(_))
  }
  
  def removeOneLeadingSpace(xs: List[String]): List[String] = {
    def startsWithSpace(s: String) = (s == "" || s.startsWith(" "))
    if (xs.forall(startsWithSpace(_))) xs.map(_.drop(1))
    else xs
  }
  
  def compactifyCell(bs: List[Block]): List[Block] = {
    compactify(List(bs)).head
  }
  
  def gridTableFooter: Parser[String] = blankLines
  
  
  
  val entityMap: Map[String, Char] = Map(
    "quot" -> '"', //  = quotation mark (= APL quote)
    "amp" -> '\u0026', // & = ampersand
    "apos" -> '\u0027', // ' = apostrophe (= apostrophe-quote); see below
    "lt" -> '\u003C', // < = less-than sign
    "gt" -> '\u003E', // > = greater-than sign
    "nbsp" -> '\u00A0', //   = no-break space (= non-breaking space)[d]
    "iexcl" -> '\u00A1', // ¡ = inverted exclamation mark
    "cent" -> '\u00A2', // ¢ = cent sign
    "pound" -> '\u00A3', // £ = pound sign
    "curren" -> '\u00A4', // ¤ = currency sign
    "yen" -> '\u00A5', // ¥ = yen sign (= yuan sign)
    "brvbar" -> '\u00A6', // ¦ = broken bar (= broken vertical bar)
    "sect" -> '\u00A7', // § = section sign
    "uml" -> '\u00A8', // ¨ = diaeresis (= spacing diaeresis); see Germanic umlaut
    "copy" -> '\u00A9', // © = copyright symbol
    "ordf" -> '\u00AA', // ª = feminine ordinal indicator
    "laquo" -> '\u00AB', // « = left-pointing double angle quotation mark
    "not" -> '\u00AC', // ¬ = not sign
    "shy" -> '\u00AD', //   = soft hyphen (= discretionary hyphen)
    "reg" -> '\u00AE', // ® = registered sign ( = registered trademark symbol)
    "macr" -> '\u00AF', // ¯ = macron (= spacing macron = overline = APL overbar)
    "deg" -> '\u00B0', // ° = degree symbol
    "plusmn" -> '\u00B1', // ± = plus-minus sign (= plus-or-minus sign)
    "sup2" -> '\u00B2', // ² = superscript two (= superscript digit two = squared)
    "sup3" -> '\u00B3', // ³ = superscript three (= superscript digit three = cubed)
    "acute" -> '\u00B4', // ´ = acute accent (= spacing acute)
    "micro" -> '\u00B5', // µ = micro sign
    "para" -> '\u00B6', // ¶ = pilcrow sign ( = paragraph sign)
    "middot" -> '\u00B7', // · = middle dot (= Georgian comma = Greek middle dot)
    "cedil" -> '\u00B8', // ¸ = cedilla (= spacing cedilla)
    "sup1" -> '\u00B9', // ¹ = superscript one (= superscript digit one)
    "ordm" -> '\u00BA', // º = masculine ordinal indicator
    "raquo" -> '\u00BB', // » = right-pointing double angle quotation mark (= right pointing guillemet)
    "frac14" -> '\u00BC', // ¼ = vulgar fraction one quarter (= fraction one quarter)
    "frac12" -> '\u00BD', // ½ = vulgar fraction one half (= fraction one half)
    "frac34" -> '\u00BE', // ¾ = vulgar fraction three quarters (= fraction three quarters)
    "iquest" -> '\u00BF', // ¿ = inverted question mark (= turned question mark)
    "Agrave" -> '\u00C0', // À = Latin capital letter A with grave accent (= Latin capital letter A grave)
    "Aacute" -> '\u00C1', // Á = Latin capital letter A with acute accent
    "Acirc" -> '\u00C2', // Â = Latin capital letter A with circumflex
    "Atilde" -> '\u00C3', // Ã = Latin capital letter A with tilde
    "Auml" -> '\u00C4', // Ä = Latin capital letter A with diaeresis
    "Aring" -> '\u00C5', // Å = Latin capital letter A with ring above (= Latin capital letter A ring)
    "AElig" -> '\u00C6', // Æ = Latin capital letter AE (= Latin capital ligature AE)
    "Ccedil" -> '\u00C7', // Ç = Latin capital letter C with cedilla
    "Egrave" -> '\u00C8', // È = Latin capital letter E with grave accent
    "Eacute" -> '\u00C9', // É = Latin capital letter E with acute accent
    "Ecirc" -> '\u00CA', // Ê = Latin capital letter E with circumflex
    "Euml" -> '\u00CB', // Ë = Latin capital letter E with diaeresis
    "Igrave" -> '\u00CC', // Ì = Latin capital letter I with grave accent
    "Iacute" -> '\u00CD', // Í = Latin capital letter I with acute accent
    "Icirc" -> '\u00CE', // Î = Latin capital letter I with circumflex
    "Iuml" -> '\u00CF', // Ï = Latin capital letter I with diaeresis
    "ETH" -> '\u00D0', // Ð = Latin capital letter Eth
    "Ntilde" -> '\u00D1', // Ñ = Latin capital letter N with tilde
    "Ograve" -> '\u00D2', // Ò = Latin capital letter O with grave accent
    "Oacute" -> '\u00D3', // Ó = Latin capital letter O with acute accent
    "Ocirc" -> '\u00D4', // Ô = Latin capital letter O with circumflex
    "Otilde" -> '\u00D5', // Õ = Latin capital letter O with tilde
    "Ouml" -> '\u00D6', // Ö = Latin capital letter O with diaeresis
    "times" -> '\u00D7', // × = multiplication sign
    "Oslash" -> '\u00D8', // Ø = Latin capital letter O with stroke (= Latin capital letter O slash)
    "Ugrave" -> '\u00D9', // Ù = Latin capital letter U with grave accent
    "Uacute" -> '\u00DA', // Ú = Latin capital letter U with acute accent
    "Ucirc" -> '\u00DB', // Û = Latin capital letter U with circumflex
    "Uuml" -> '\u00DC', // Ü = Latin capital letter U with diaeresis
    "Yacute" -> '\u00DD', // Ý = Latin capital letter Y with acute accent
    "THORN" -> '\u00DE', // Þ = Latin capital letter THORN
    "szlig" -> '\u00DF', // ß = Latin small letter sharp s (= ess-zed); see German Eszett
    "agrave" -> '\u00E0', // à = Latin small letter a with grave accent
    "aacute" -> '\u00E1', // á = Latin small letter a with acute accent
    "acirc" -> '\u00E2', // â = Latin small letter a with circumflex
    "atilde" -> '\u00E3', // ã = Latin small letter a with tilde
    "auml" -> '\u00E4', // ä = Latin small letter a with diaeresis
    "aring" -> '\u00E5', // å = Latin small letter a with ring above
    "aelig" -> '\u00E6', // æ = Latin small letter ae (= Latin small ligature ae)
    "ccedil" -> '\u00E7', // ç = Latin small letter c with cedilla
    "egrave" -> '\u00E8', // è = Latin small letter e with grave accent
    "eacute" -> '\u00E9', // é = Latin small letter e with acute accent
    "ecirc" -> '\u00EA', // ê = Latin small letter e with circumflex
    "euml" -> '\u00EB', // ë = Latin small letter e with diaeresis
    "igrave" -> '\u00EC', // ì = Latin small letter i with grave accent
    "iacute" -> '\u00ED', // í = Latin small letter i with acute accent
    "icirc" -> '\u00EE', // î = Latin small letter i with circumflex
    "iuml" -> '\u00EF', // ï = Latin small letter i with diaeresis
    "eth" -> '\u00F0', // ð = Latin small letter eth
    "ntilde" -> '\u00F1', // ñ = Latin small letter n with tilde
    "ograve" -> '\u00F2', // ò = Latin small letter o with grave accent
    "oacute" -> '\u00F3', // ó = Latin small letter o with acute accent
    "ocirc" -> '\u00F4', // ô = Latin small letter o with circumflex
    "otilde" -> '\u00F5', // õ = Latin small letter o with tilde
    "ouml" -> '\u00F6', // ö = Latin small letter o with diaeresis
    "divide" -> '\u00F7', // ÷ = division sign (= obelus)
    "oslash" -> '\u00F8', // ø = Latin small letter o with stroke (= Latin small letter o slash)
    "ugrave" -> '\u00F9', // ù = Latin small letter u with grave accent
    "uacute" -> '\u00FA', // ú = Latin small letter u with acute accent
    "ucirc" -> '\u00FB', // û = Latin small letter u with circumflex
    "uuml" -> '\u00FC', // ü = Latin small letter u with diaeresis
    "yacute" -> '\u00FD', // ý = Latin small letter y with acute accent
    "thorn" -> '\u00FE', // þ = Latin small letter thorn
    "yuml" -> '\u00FF', // ÿ = Latin small letter y with diaeresis
    "OElig" -> '\u0152', // Œ = Latin capital ligature oe[e]
    "oelig" -> '\u0153', // œ = Latin small ligature oe[e]
    "Scaron" -> '\u0160', // Š = Latin capital letter s with caron
    "scaron" -> '\u0161', // š = Latin small letter s with caron
    "Yuml" -> '\u0178', // Ÿ = Latin capital letter y with diaeresis
    "fnof" -> '\u0192', // ƒ = Latin small letter f with hook (= function = florin)
    "circ" -> '\u02C6', // ˆ = modifier letter circumflex accent
    "tilde" -> '\u02DC', // ˜ = small tilde
    "Alpha" -> '\u0391', // Α = Greek capital letter Alpha
    "Beta" -> '\u0392', // Β = Greek capital letter Beta
    "Gamma" -> '\u0393', // Γ = Greek capital letter Gamma
    "Delta" -> '\u0394', // Δ = Greek capital letter Delta
    "Epsilon" -> '\u0395', // Ε = Greek capital letter Epsilon
    "Zeta" -> '\u0396', // Ζ = Greek capital letter Zeta
    "Eta" -> '\u0397', // Η = Greek capital letter Eta
    "Theta" -> '\u0398', // Θ = Greek capital letter Theta
    "Iota" -> '\u0399', // Ι = Greek capital letter Iota
    "Kappa" -> '\u039A', // Κ = Greek capital letter Kappa
    "Lambda" -> '\u039B', // Λ = Greek capital letter Lambda
    "Mu" -> '\u039C', // Μ = Greek capital letter Mu
    "Nu" -> '\u039D', // Ν = Greek capital letter Nu
    "Xi" -> '\u039E', // Ξ = Greek capital letter Xi
    "Omicron" -> '\u039F', // Ο = Greek capital letter Omicron
    "Pi" -> '\u03A0', // Π = Greek capital letter Pi
    "Rho" -> '\u03A1', // Ρ = Greek capital letter Rho
    "Sigma" -> '\u03A3', // Σ = Greek capital letter Sigma
    "Tau" -> '\u03A4', // Τ = Greek capital letter Tau
    "Upsilon" -> '\u03A5', // Υ = Greek capital letter Upsilon
    "Phi" -> '\u03A6', // Φ = Greek capital letter Phi
    "Chi" -> '\u03A7', // Χ = Greek capital letter Chi
    "Psi" -> '\u03A8', // Ψ = Greek capital letter Psi
    "Omega" -> '\u03A9', // Ω = Greek capital letter Omega
    "alpha" -> '\u03B1', // α = Greek small letter alpha
    "beta" -> '\u03B2', // β = Greek small letter beta
    "gamma" -> '\u03B3', // γ = Greek small letter gamma
    "delta" -> '\u03B4', // δ = Greek small letter delta
    "epsilon" -> '\u03B5', // ε = Greek small letter epsilon
    "zeta" -> '\u03B6', // ζ = Greek small letter zeta
    "eta" -> '\u03B7', // η = Greek small letter eta
    "theta" -> '\u03B8', // θ = Greek small letter theta
    "iota" -> '\u03B9', // ι = Greek small letter iota
    "kappa" -> '\u03BA', // κ = Greek small letter kappa
    "lambda" -> '\u03BB', // λ = Greek small letter lambda
    "mu" -> '\u03BC', // μ = Greek small letter mu
    "nu" -> '\u03BD', // ν = Greek small letter nu
    "xi" -> '\u03BE', // ξ = Greek small letter xi
    "omicron" -> '\u03BF', // ο = Greek small letter omicron
    "pi" -> '\u03C0', // π = Greek small letter pi
    "rho" -> '\u03C1', // ρ = Greek small letter rho
    "sigmaf" -> '\u03C2', // ς = Greek small letter final sigma
    "sigma" -> '\u03C3', // σ = Greek small letter sigma
    "tau" -> '\u03C4', // τ = Greek small letter tau
    "upsilon" -> '\u03C5', // υ = Greek small letter upsilon
    "phi" -> '\u03C6', // φ = Greek small letter phi
    "chi" -> '\u03C7', // χ = Greek small letter chi
    "psi" -> '\u03C8', // ψ = Greek small letter psi
    "omega" -> '\u03C9', // ω = Greek small letter omega
    "thetasym" -> '\u03D1', // ϑ = Greek theta symbol
    "upsih" -> '\u03D2', // ϒ = Greek Upsilon with hook symbol
    "piv" -> '\u03D6', // ϖ = Greek pi symbol
    "ensp" -> '\u2002', //   = en space[d]
    "emsp" -> '\u2003', //   = em space[d]
    "thinsp" -> '\u2009', //   = thin space[d]
    "zwnj" -> '\u200C', //   = zero-width non-joiner
    "zwj" -> '\u200D', //   = zero-width joiner
    "lrm" -> '\u200E', //   = left-to-right mark
    "rlm" -> '\u200F', //   = right-to-left mark
    "ndash" -> '\u2013', // – = en dash
    "mdash" -> '\u2014', // — = em dash
    "lsquo" -> '\u2018', // ‘ = left single quotation mark
    "rsquo" -> '\u2019', // ’ = right single quotation mark
    "sbquo" -> '\u201A', // ‚ = single low-9 quotation mark
    "ldquo" -> '\u201C', // “ = left double quotation mark
    "rdquo" -> '\u201D', // ” = right double quotation mark
    "bdquo" -> '\u201E', // „ = double low-9 quotation mark
    "dagger" -> '\u2020', // † = dagger, obelisk
    "Dagger" -> '\u2021', // ‡ = double dagger, double obelisk
    "bull" -> '\u2022', // • = bullet (= black small circle)[f]
    "hellip" -> '\u2026', // … = horizontal ellipsis (= three dot leader)
    "permil" -> '\u2030', // ‰ = per mille sign
    "prime" -> '\u2032', // ′ = prime (= minutes = feet)
    "Prime" -> '\u2033', // ″ = double prime (= seconds = inches)
    "lsaquo" -> '\u2039', // ‹ = single left-pointing angle quotation mark[g]
    "rsaquo" -> '\u203A', // › = single right-pointing angle quotation mark[g]
    "oline" -> '\u203E', // ‾ = overline (= spacing overscore)
    "frasl" -> '\u2044', // ⁄ = fraction slash (= solidus)
    "euro" -> '\u20AC', // € = euro sign
    "image" -> '\u2111', // ℑ = black-letter capital I (= imaginary part)
    "weierp" -> '\u2118', // ℘ = script capital P (= power set = Weierstrass p)
    "real" -> '\u211C', // ℜ = black-letter capital R (= real part symbol)
    "trade" -> '\u2122', // ™ = trademark symbol
    "alefsym" -> '\u2135', // ℵ = alef symbol (= first transfinite cardinal)[h]
    "larr" -> '\u2190', // ← = leftwards arrow
    "uarr" -> '\u2191', // ↑ = upwards arrow
    "rarr" -> '\u2192', // → = rightwards arrow
    "darr" -> '\u2193', // ↓ = downwards arrow
    "harr" -> '\u2194', // ↔ = left right arrow
    "crarr" -> '\u21B5', // ↵ = downwards arrow with corner leftwards (= carriage return)
    "lArr" -> '\u21D0', // ⇐ = leftwards double arrow[i]
    "uArr" -> '\u21D1', // ⇑ = upwards double arrow
    "rArr" -> '\u21D2', // ⇒ = rightwards double arrow[j]
    "dArr" -> '\u21D3', // ⇓ = downwards double arrow
    "hArr" -> '\u21D4', // ⇔ = left right double arrow
    "forall" -> '\u2200', // ∀ = for all
    "part" -> '\u2202', // ∂ = partial differential
    "exist" -> '\u2203', // ∃ = there exists
    "empty" -> '\u2205', // ∅ = empty set (= null set = diameter)
    "nabla" -> '\u2207', // ∇ = nabla (= backward difference)
    "isin" -> '\u2208', // ∈ = element of
    "notin" -> '\u2209', // ∉ = not an element of
    "ni" -> '\u220B', // ∋ = contains as member
    "prod" -> '\u220F', // ∏ = n-ary product (= product sign)[k]
    "sum" -> '\u2211', // ∑ = n-ary summation[l]
    "minus" -> '\u2212', // − = minus sign
    "lowast" -> '\u2217', // ∗ = asterisk operator
    "radic" -> '\u221A', // √ = square root (= radical sign)
    "prop" -> '\u221D', // ∝ = proportional to
    "infin" -> '\u221E', // ∞ = infinity
    "ang" -> '\u2220', // ∠ = angle
    "and" -> '\u2227', // ∧ = logical and (= wedge)
    "or" -> '\u2228', // ∨ = logical or (= vee)
    "cap" -> '\u2229', // ∩ = intersection (= cap)
    "cup" -> '\u222A', // ∪ = union (= cup)
    "int" -> '\u222B', // ∫ = integral
    "there4" -> '\u2234', // ∴ = therefore sign
    "sim" -> '\u223C', // ∼ = tilde operator (= varies with = similar to)[m]
    "cong" -> '\u2245', // ≅ = congruent to
    "asymp" -> '\u2248', // ≈ = almost equal to (= asymptotic to)
    "ne" -> '\u2260', // ≠ = not equal to
    "equiv" -> '\u2261', // ≡ = identical to; sometimes used for 'equivalent to'
    "le" -> '\u2264', // ≤ = less-than or equal to
    "ge" -> '\u2265', // ≥ = greater-than or equal to
    "sub" -> '\u2282', // ⊂ = subset of
    "sup" -> '\u2283', // ⊃ = superset of[n]
    "nsub" -> '\u2284', // ⊄ = not a subset of
    "sube" -> '\u2286', // ⊆ = subset of or equal to
    "supe" -> '\u2287', // ⊇ = superset of or equal to
    "oplus" -> '\u2295', // ⊕ = circled plus (= direct sum)
    "otimes" -> '\u2297', // ⊗ = circled times (= vector product)
    "perp" -> '\u22A5', // ⊥ = up tack (= orthogonal to = perpendicular)[o]
    "sdot" -> '\u22C5', // ⋅ = dot operator[p]
    "lceil" -> '\u2308', // ⌈ = left ceiling (= APL upstile)
    "rceil" -> '\u2309', // ⌉ = right ceiling
    "lfloor" -> '\u230A', // ⌊ = left floor (= APL downstile)
    "rfloor" -> '\u230B', // ⌋ = right floor
    "lang" -> '\u2329', // 〈 = left-pointing angle bracket (= bra)[q]
    "rang" -> '\u232A', // 〉 = right-pointing angle bracket (= ket)[r]
    "loz" -> '\u25CA', // ◊ = lozenge
    "spades" -> '\u2660', // ♠ = black spade suit[f]
    "clubs" -> '\u2663', // ♣ = black club suit (= shamrock)[f]
    "hearts" -> '\u2665', // ♥ = black heart suit (= valentine)[f]
    "diams" -> '\u2666' // ♦ = black diamond suit[f]
  )
}