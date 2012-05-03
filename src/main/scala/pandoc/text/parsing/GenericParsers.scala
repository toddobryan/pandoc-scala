package pandoc.text.parsing

import genparser.{ChoiceParser, Parser, Pos, GenericReader, Reader, Result, Ok, Error, Failure, State, Success, <~>}

import genparser.CharParsers._

import pandoc.text._

import pandoc.text.parsing.Shared._

object GenericParsers {
  // many1Till(p, end) => (p +) <~ end
  // notFollowedBy'(p) => not(p)
    
  def anyLine: Parser[String, ParserState, Char] = {
    (noneOf("\n").* <~ lit('\n')) ^^ (_.mkString)
  }
  def spaceChar: Parser[Char, ParserState, Char] = oneOf(" \t")
  def nonSpaceChar: Parser[Char, ParserState, Char] = noneOf("\t\n \r")
  def skipSpaces: Parser[Null, ParserState, Char] = (spaceChar *) ^^^ null
  def blankLine: Parser[Char, ParserState, Char] = skipSpaces ~> lit('\n')
  def blankLines: Parser[String, ParserState, Char] = (blankLine.+) ^^ ((cs: List[Char]) => cs.mkString)
  
  def enclosed[A](start: Parser[Any, ParserState, Char], 
                  end: Parser[Any, ParserState, Char],
                  content: Parser[A, ParserState, Char]): Parser[List[A], ParserState, Char] = {
    start.notFollowedBy(lit(' ')) ~> (content +) <~ end
  }
  
  def anyCaseListOfChars(chars: List[Char]): Parser[List[Char], ParserState, Char] = {
    chars match {
      case Nil => new Success(Nil)
      case c :: cs => ((lit[ParserState](c.toLower) | lit[ParserState](c.toUpper)) <~> anyCaseListOfChars(cs)) ^^ {
        case fst <~> rst => fst :: rst
      }
    }
  }
  
  def stringAnyCase(str: String): Parser[String, ParserState, Char] = {
    anyCaseListOfChars(str.toList) ^^ ((cs: List[Char]) => cs.mkString)
  }
  
  def parseFromString[A, State, Elem](parser: Parser[A, State, Elem], str: List[Elem]) = new Parser[A, State, Elem] {
    def apply(state: State, in: Reader[Elem]): Result[A, State, Elem] = {
      parser.apply(state, new GenericReader[Elem](Stream(str: _*), Pos(1, 1))) match {
        case Ok(res, newState, _) => Ok(res, newState, in)
        case Error(newState, _, msgs) => Error(newState, in, msgs)
      }
    }
  }
  
  def lineClump: Parser[String, ParserState, Char] = {
    blankLines |
    (((blankLine.X ~> anyLine) +) ^^ ((lines: List[String]) => lines.mkString("\n")))
  }
  
  def listOfCharsInBalanced(open: Char, close: Char, 
                      content: => Parser[Char, ParserState, Char]): Parser[List[Char], ParserState, Char] = {
    def contentWithoutDelims: Parser[List[Char], ParserState, Char] = {
      ((lit[ParserState](open) | lit[ParserState](close)).X ~> content) +
    }
    def raw: Parser[List[Char], ParserState, Char] = {
      contentWithoutDelims | (listOfCharsInBalanced(open, close, content) ^^ ((res: List[Char]) => open +: res :+ close))
    }
    (lit(open) ~> raw <~ lit(close))
  }
  
  def charInBalanced(open: Char, close: Char, content: => Parser[Char, ParserState, Char]): Parser[String, ParserState, Char] = {
    listOfCharsInBalanced(open, close, content) ^^ ((cs: List[Char]) => cs.mkString)
  }
  
  def romanNumeral(isUpper: Boolean): Parser[Int, ParserState, Char] = {
    val lowerRomanDigits = List('i', 'v', 'x', 'l', 'c', 'd', 'm')
    val romanDigits: List[Char] = {
      if (!isUpper) lowerRomanDigits else lowerRomanDigits.map(_.toUpper)
    }
    val i = lit(romanDigits(0))
    val v = lit(romanDigits(1))
    val x = lit(romanDigits(2))
    val l = lit(romanDigits(3))
    val c = lit(romanDigits(4))
    val d = lit(romanDigits(5))
    val m = lit(romanDigits(6))
    val thousands: Parser[Int, ParserState, Char] = m.count(0, 3) ^^ ((ms: List[Char]) => 1000 * ms.length)
    def genericRoman(ten: Parser[Char, ParserState, Char],
                     five: Parser[Char, ParserState, Char],
                     one: Parser[Char, ParserState, Char],
                     mult: Int): Parser[Int, ParserState, Char] = {
      oneOf(romanDigits.mkString).lookAhead ~>
      ((one <~> ten ^^^ 9 * mult)
       | (one <~> five ^^^ 4 * mult)
       | ((five.count(0, 1) ^^ ((fs: List[Char]) => 5 * mult * fs.length)) <~>
          ((one.count(0, 3)) ^^ ((os: List[Char]) => 1 * mult * os.length))) ^^ {
            case fv <~> on => fv + on
         })
    }
    val hundreds: Parser[Int, ParserState, Char] = genericRoman(m, d, c, 100)
    val tens: Parser[Int, ParserState, Char] = genericRoman(c, l, x, 10)
    val ones: Parser[Int, ParserState, Char] = genericRoman(x, v, i, 1)
    (thousands <~> hundreds <~> tens <~> ones) ^^ {
      case ((th: Int) <~> (hu: Int) <~> (te: Int) <~> (on: Int)) => th + hu + te + on
    }
  }
  
  def domain: Parser[String, ParserState, Char] = {
    val domainChar = alphaNum | lit('-')
    """%s+(\.%s+)+""".format(domainChar, domainChar).r
    domainChar.+ <~> ((lit('.') <~> domainChar.+) ^^ { case dot <~> name => (dot :: name).mkString}).+ ^^ {
      case name <~> names => name.mkString + names.mkString
    }
  }
  
  def emailAddress: Parser[(String, String), ParserState, Char] = {
    val addr: Parser[String, ParserState, Char] = (alphaNum <~> (alphaNum | oneOf("-+_.")).*) ^^ {
      case fst <~> rst => (fst :: rst).mkString
    }
    (addr <~ lit('@') <~> domain) ^^ {
      case ad <~> dom => val email = ad + "@" + dom; (email, "mailto:" + email)
    }
  }
  
  /*def uri(implicit state: ParserState): Parser[(String, String)] = {
    //TODO isAllowedInURI
    val protocols = """(https?|ftp|file|mailto|news|telnet):""".r
    protocols.map((s: String) => (s, s))
  }*/
  
  def withHorizDisplacement[A](parser: Parser[A, ParserState, Char]): Parser[(A, Int), ParserState, Char] = {
    new Parser[(A, Int), ParserState, Char] {
      def apply(state: ParserState, in: Reader[Char]): Result[(A, Int), ParserState, Char] = {
        val col1 = in.currPos.column
        parser(state, in) match {
          case Ok(a, newState, next) => val col2 = next.currPos.column; Ok((a, col2 - col1), newState, next)
          case e: Error[_, _] => e
        }
      }
    }
  }
  
  def withRaw[A](parser: Parser[A, ParserState, Char]): Parser[(A, String), ParserState, Char] = {
    new Parser[(A, String), ParserState, Char] {
      def apply(state: ParserState, in: Reader[Char]): Result[(A, String), ParserState, Char] = {
        parser(state, in) match {
          case Ok(a, newState, next) => {
            def accChars(r: Reader[Char], cs: List[Char]): List[Char] = {
              if (in.currPos == next.currPos) cs.reverse
              else accChars(r.rest, r.first :: cs)
            }
            val raw = accChars(in, Nil).mkString
            Ok((a, raw), newState, next)
          }
          case e: Error[_, _] => e
        }
      }
    }
  }
  
  def nullBlock: Parser[Block, ParserState, Char] = anyChar ^^^ EmptyBlock
  
  def failIfStrict[Elem] = new Parser[Unit, ParserState, Elem] {
    def apply(state: ParserState, in: Reader[Elem]): Result[Unit, ParserState, Elem] = {
      if (state.strict) Error(state, in, "strict mode")
      else Ok((), state, in)
    }
  }
  
  def failUnlessLHS[Elem] = new Parser[Unit, ParserState, Elem] {
    def apply(state: ParserState, in: Reader[Elem]): Result[Unit, ParserState, Elem] = {
      if (state.literateHaskell) Ok((), state, in)
      else Error(state, in, "not in literate Haskell mode")
    }
  }
  
  def escaped(parser: Parser[Char, ParserState, Char]) = lit('\\') ~> parser
    
  def characterReference = new Parser[Char, ParserState, Char] {
    def apply(state: ParserState, in: Reader[Char]): Result[Char, ParserState, Char] = {
      (lit('&') ~> noneOf[ParserState](" ;").+ <~ lit(';'))(state, in) match {
        case Ok(res, newState, next) => entityMap.get(res.mkString) match {
          case Some(s) => Ok(s, newState, next)
          case None => Error(state, in, "entity not found")
        }
        case e: Error[_, _] => e
      }
    }
  }
  
  def upperRoman: Parser[(ListNumberStyle, Int), ParserState, Char] = {
    romanNumeral(true) ^^ ((num: Int) => (UpperRoman, num))
  }
  
  def lowerRoman: Parser[(ListNumberStyle, Int), ParserState, Char] = {
    romanNumeral(false) ^^ ((num: Int) => (LowerRoman, num))
  }
  
  def decimal: Parser[(ListNumberStyle, Int), ParserState, Char] = {
    digit.+ ^^ ((s: List[Char]) => (Decimal, s.mkString.toInt))
  }
  

  def exampleNum = new Parser[(ListNumberStyle, Int), ParserState, Char] {
    def apply(state: ParserState, in: Reader[Char]): Result [(ListNumberStyle, Int), ParserState, Char] = {
      (lit('@') ~> ((alphaNum[ParserState] | oneOf[ParserState]("_-")).* ^^ ((cs: List[Char]) => cs.mkString)))(state, in) match {
        case Ok(label, newState: ParserState, next) => {
          val num: Int = newState.nextExample
          val newLabels: Map[String, Int] = if (label == "") newState.examples
          									else newState.examples + (label -> num)
          val newerState: ParserState = newState.copy(nextExample = num + 1, examples = newLabels)
          Ok((Example, num), newerState, next)
        }
        case e: Error[_, _] => e
      }
    }
  }
  
  def defaultNum: Parser[(ListNumberStyle, Int), ParserState, Char] = {
    lit[ParserState]('#') ^^^ (DefaultStyle, 1)
  }
  
  def lowerAlpha: Parser[(ListNumberStyle, Int), ParserState, Char] = {
    val lowerChars: String = "abcdefghijklmnopqrstuvwxyz"
    oneOf(lowerChars) ^^ ((c: Char) => (LowerAlpha, lowerChars.indexOf(c) + 1))
  }
  
  def upperAlpha: Parser[(ListNumberStyle, Int), ParserState, Char] = {
    val upperChars: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    oneOf(upperChars) ^^ ((c: Char) => (UpperAlpha, upperChars.indexOf(c) + 1))
  }

  def romanOne: Parser[(ListNumberStyle, Int), ParserState, Char] = {
    (lit('i') ^^^ (LowerRoman, 1)) | (lit('I') ^^^ (UpperRoman, 1))
  }
  
  def anyOrderedListMarker: Parser[ListAttributes, ParserState, Char] = {
    val delims: List[(Parser[(ListNumberStyle, Int), ParserState, Char] => Parser[ListAttributes, ParserState, Char])] =
      List(inPeriod _, inOneParen, inTwoParens _)
    val numParsers: List[Parser[(ListNumberStyle, Int), ParserState, Char]] =
      List(decimal, exampleNum, defaultNum, romanOne, lowerAlpha, 
           lowerRoman, upperAlpha, upperRoman)
    val listParsers: List[Parser[ListAttributes, ParserState, Char]] = {
      delims.flatMap((d: (Parser[(ListNumberStyle, Int), ParserState, Char] =>
        Parser[ListAttributes, ParserState, Char])) => {
          numParsers.map(d(_))
      })
    }
    new ChoiceParser(listParsers: _*)
  }
  
  def inPeriod(numParser: Parser[(ListNumberStyle, Int), ParserState, Char]): Parser[ListAttributes, ParserState, Char] = {
    (numParser <~ lit('.')) ^^ {
      case (style: ListNumberStyle, start: Int) => {
        val delim = if (style == DefaultStyle) DefaultDelim else Period
        ListAttributes(start, style, delim)
      }
    }
  }
  
  def inOneParen(numParser: Parser[(ListNumberStyle, Int), ParserState, Char]): Parser[ListAttributes, ParserState, Char] = {
    (numParser <~ lit(')')) ^^ {
      case (style, start) => ListAttributes(start, style, OneParen)
    }
  }
  
  def inTwoParens(numParser: Parser[(ListNumberStyle, Int), ParserState, Char]): Parser[ListAttributes, ParserState, Char] = {
    (lit('(') ~> numParser <~ lit(')')) ^^ {
      case (style, start) => ListAttributes(start, style, TwoParens)
    }
  }
  
  def orderedListMarker(style: ListNumberStyle, delim: ListNumberDelim): Parser[Int, ParserState, Char] = {
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
    context(num) ^^ {
      case ListAttributes(start, _, _) => start
    }
  }
  
  def charRef: Parser[Inline, ParserState, Char] = characterReference ^^ ((c: Char) => Str(c.toString))
  
  def tableWith[Sep, End](headerParser: Parser[(List[TableCell], List[Alignment], List[Int]), ParserState, Char],
      rowParser: (List[Int]) => Parser[List[TableCell], ParserState, Char],
      lineParser: Parser[Sep, ParserState, Char],
      footerParser: Parser[End, ParserState, Char],
      captionParser: Parser[List[Inline], ParserState, Char]): Parser[Block, ParserState, Char] = {
    for {
      captionPrime <- captionParser.orElse(Nil)
      headsAndAlignsAndIndices <- headerParser
      val (heads, aligns, indices) = headsAndAlignsAndIndices
      linesPrime <- rowParser(indices).sepEndBy(lineParser)
      footer <- footerParser
      caption <- if (captionPrime.isEmpty) captionParser.orElse(Nil) else new Success(captionPrime)
      state <- new State[ParserState, ParserState, Char]()
      val widths = widthsFromIndices(state.columns, indices)
    } yield Table(caption, aligns, widths, heads, linesPrime)    
  }
  
  def widthsFromIndices(numColumnsPrime: Int, indices: List[Int]): List[Double] = {
    if (indices.isEmpty) Nil
    else {
      import scala.Math.max
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
  
  def gridTableWith(block: Parser[Block, ParserState, Char],
      tableCaption: Parser[List[Inline], ParserState, Char],
      isHeadless: Boolean): Parser[Block, ParserState, Char] = {
    tableWith(gridTableHeader(isHeadless, block), gridTableRow(block), gridTableSep('-'), 
              gridTableFooter, tableCaption)
  }
  
  def gridTableSplitLine(indices: List[Int], line: String): List[String] = {
    splitStringByIndices(indices.init, removeTrailingSpace(line))
  }
  
  def gridPart(c: Char): Parser[(Int, Int), ParserState, Char] = {
    for {
      dashes <- lit(c).+
      foo <- lit('+')
    } yield (dashes.length, dashes.length + 1)
  }
  
  def gridDashedLines(c: Char): Parser[List[(Int, Int)], ParserState, Char] = {
    lit('+') ~> gridPart(c).+ <~ blankLine
  }
  
  def removeFinalBar(s: String): String = {
    s.reverse.dropWhile(" \t".contains(_)).dropWhile(_ == '|').reverse
  }
  
  def gridTableSep(c: Char): Parser[Char, ParserState, Char] = {
    gridDashedLines(c) ^^^ '\n'
  }
  
  def mapM[A, B, State, Elem](f: A => Parser[B, State, Elem], as: List[A]): Parser[List[B], State, Elem] = {
    as match {
      case Nil => new Success(Nil)
      case first :: rest => for {
        b <- f(first)
        bs <- mapM(f, rest)
      } yield b :: bs
    }
  }
  
  def gridTableHeader(isHeadless: Boolean, block: Parser[Block, ParserState, Char]): Parser[(List[List[Block]], List[Alignment], List[Int]), ParserState, Char] = {
    for {
      filler1 <- blankLines.?
      dashes <- gridDashedLines('-')
      rawContent <- if (isHeadless) new Success(Nil) else (gridTableSep('=').X ~> lit('|') ~> (anyChar.+ ^^ ((cs: List[Char]) => cs.mkString)) <~ lit('\n')).+
      filler2 <- if (isHeadless) new Success(Unit) else (gridTableSep('=') ^^^ Unit)
      val linesPrime = dashes.map(_._2)
      val indices = linesPrime.scanLeft(0)((x: Int, y: Int) => x + y)
      val aligns = List.fill(linesPrime.length)(AlignDefault)
      val rawHeads = if (isHeadless) {
        List.fill(dashes.length)("")
      } else {
        rawContent.map(gridTableSplitLine(indices, _)).transpose.map(_.mkString(" "))
      }
      heads <- mapM((s: String) => parseFromString[List[Block], ParserState, Char](block.*, s.toList),
      		        rawHeads.map(removeLeadingTrailingSpace(_)))
    } yield (heads, aligns, indices)
  }
  
  def gridTableRawLine(indices: List[Int]): Parser[List[String], ParserState, Char] = {
    for {
      filler <- lit('|')
      line <- (anyChar.+ ^^ ((cs: List[Char]) => cs.mkString)) <~ lit('\n')
    } yield gridTableSplitLine(indices, line)
  }
  
  def gridTableRow(block: Parser[Block, ParserState, Char], indices: List[Int]): Parser[List[List[Block]], ParserState, Char] = {
    def stringParser(str: String): Parser[List[Block], ParserState, Char] = {
      parseFromString(block.*, str.toList) ^^ (compactifyCell _)
    }
    for {
      colLines <- gridTableRawLine(indices).+
      val cols = colLines.transpose.map((strs: List[String]) => removeOneLeadingSpace(strs).mkString("\n") + "\n")
      parser <- mapM((stringParser _), cols)
    } yield parser
  }
  
  def removeOneLeadingSpace(xs: List[String]): List[String] = {
    if (xs.forall((s: String) => s.startsWith(" ") || s == "")) {
      xs.map((s: String) => s.toList.drop(1).mkString)
    } else {
      xs
    }
  }
  
  def compactifyCell(bs: List[Block]): List[Block] = {
    compactify(List(bs)).head
  }
  
  def gridTableFooter = blankLines
  
  def readWith[A, Elem](parser: Parser[A, ParserState, Elem], state: ParserState, in: Reader[Elem]): A = {
    parser(state, in) match {
      case Ok(a, _, _) => a
      case Error(_, _, msgs) => throw new Exception(msgs.mkString("\n"))
    }
  }
  
  // def testStringWith[]
  // don't think we need, since it's for testing
  
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