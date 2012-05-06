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
      foo <- lit('|')
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
  

}