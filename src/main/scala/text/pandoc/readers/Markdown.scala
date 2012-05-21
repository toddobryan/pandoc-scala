package pandoc.text.readers

import scala.util.parsing.input.CharSequenceReader
import scala.math.max

import pandoc.text._
import pandoc.text.Shared.{compactify, normalizeSpaces, stripTrailingNewlines}
import pandoc.text.Definition._

object Markdown extends Parsing {
  
  /*def readMarkdown(state: ParserState, doc: String): Pandoc = {
    parse(parseMarkdown, new StatefulReader(state, new CharSequenceReader(doc + "\n\n")))
  }*/
  
  def isBulletListMarker(c: Char): Boolean = "*+-".contains(c)
  def isHruleChar(c: Char): Boolean = "*-_".contains(c)
  val setextHChars = "=-"
  def isBlank(c: Char): Boolean = " \t\n".contains(c)
  
  def indentSpaces: Parser[String] = {
    for {
      state <- getState
      tabStop = state.tabStop
      res <- (literal(" " * tabStop) | literal("\t")).named("indentation") 
    } yield res
  }
  
  def nonindentSpaces: Parser[String] = StatefulParser[String]{ 
    (in: StatefulReader[ParserState, Elem]) => {
      val tabStop = in.state.tabStop
      ("""[ ]*""".r)(in) match {
        case Success(sps, next) => if (sps.length < tabStop) Success(sps, next)
                                   else Failure("unexpected indented line", in)
        case x => x
      }
    }
  }
  
  def skipNonindentSpaces: Parser[Unit] = {
    for {
      state <- getState
      res <- """[ ]{0, %d}""".format(state.tabStop - 1).r
    } yield ()
  }
  
  // didn't translate atMostSpaces, but put in skipNonindentSpaces
  
  def litChar: Parser[Char] = {
    escapedCharPrime | elem("non newline", (c: Char) => c != '\n') | (elem('\n') ~> not(blankLine) ^^^ ' ')
  }
  
  def failUnlessBeginningOfLine: Parser[Unit] = {
    StatefulParser[Unit]((in: StatefulReader[ParserState, Elem]) => {
      if (in.pos.column == 1) Success((), in)
      else Failure("not beginning of line", in)
    })
  }
  
  def inlinesInBalancedBrackets(parser: Parser[Inline]): Parser[List[Inline]] = {
    def inlinesInside: Parser[List[Inline]] = {
      (guard(parser).^? { case Str("[") => () }) ~> 
        (inlinesInBalancedBrackets(parser)) ^^ ((bal: List[Inline]) => List(Str("[")) ++ bal ++ List(Str("]")))
    }
    for {
      _ <- elem('[')
      result <- (inlinesInside | (parser ^^ ((inline: Inline) => List(inline)))).* <~ elem(']')
    } yield result.flatten
  }
  
  def titleLine: Parser[List[Inline]] = {
    for {
      _ <- elem('%')
      _ <- skipSpaces
      res <- ((not(elem('\n')) ~> inline) | (endline ~> whitespace)).* <~ elem('\n')
    } yield normalizeSpaces(res)
  }
  
  def authorsLine: Parser[List[List[Inline]]] = {
    for {
      _ <- elem('%') ~ skipSpaces
      authors <- sependby( not(elem(';') | elem('\n')) ~> inline, 
                           elem(';') | (elem('\n') ~> not(blankLine) ~> spaceChar)) <~ elem('\n')
    } yield authors.map(normalizeSpaces(_)).filter(!_.isEmpty)
  }
  
  def dateLine: Parser[List[Inline]] = {
    for {
      _ <- elem('%') ~ skipSpaces
      date <- inline.* <~ elem('\n')
    } yield normalizeSpaces(date)
  }
  
  def titleBlock: Parser[(List[Inline], List[List[Inline]], List[Inline])] = {
    for {
      _ <- failIfStrict
      title <- titleLine | success(Nil)
      author <- authorsLine | success(Nil)
      date <- dateLine | success(Nil)
      _ <- blankLines.?
    } yield (title, author, date)
  }
  
  def parseMarkdown: Parser[Pandoc] = {
    def handleExampleRef(examples: Map[String, Inline])(z: Inline): Inline = {
      z match {
        case Str(xs) if (xs.startsWith("@")) => examples.get(xs.substring(1)) match {
          case Some(n) => Str(n)
          case None => xs
        }
        case _ => z
      }
    }
    for {
      _ <- updateState((s: ParserState) => s.copy(context = s.context.copy(raw = true)))
      startPos <- getPosition
      st <- getState
      firstPassParser = referenceKey | (if (st.strict) pzero else noteBlock) | lineClump
      docMinusKeys <- (firstPassParser.* <~ eof) ^^ (_.flatten)
      _ <- setInput(docMinusKeys)
      _ <- setPosition(startPos)
      stPrime <- getState
      reversedNotes = stPrime.notes
      _ <- updateState((s: ParserState) => s.copy(notes = reversedNotes.reverse))
      titleAuthorDate <- titleBlock | Success((Nil, Nil, Nil))
      (title, author, date) = titleAuthorDate
      blocks <- parseBlocks
      doc = Pandoc(Meta(title, author, date), blocks.filter(_ != EmptyBlock))
      tempState <- getState
      examples = tempState.examples
    } yield if (examples.isEmpty) doc else bottomUp(handleExampleRef(examples))(doc)
  }
  
  def sourceUrl: Parser[String] = {
    not(referenceTitle) ~> spaceChar.* ~> (elem('\n') ~ not(blankLine)).? ~>
      spaceChar.* ~> not(reference) ~> (escapedCharPrime | elem("non-blank", (c: Char) => !isBlank(c))).+
  }
  
  def betweenAngles: Parser[String] = elem('<') ~> (escapedCharPrime | litChar).* <~ elem('>')
  
  def referenceKey: Parser[String] = {
    for {
      startPos <- getPosition
      lab <- skipNonindentSpaces ~> reference <~ elem(':') <~ skipSpaces <~ elem('\n').? <~ skipSpaces <~ not(elem('['))
      src <- betweenAngles | sourceUrl
      tit <- (referenceTitle | success("")) <~ blankLines
      endPos <- getPosition
      target = (escapeUri(removeTrailingSpace(src)), tit)
      st <- getState
      oldKeys = st.keys
      _ <- updateState((s: ParserState) => s.copy(keys = oldKeys + (toKey(lab) -> target)))
    } yield "\n" * (endPos.line - startPos.line)
  }
  
  def noteMarker: Parser[String] = {
    "[^" ~> elem("non-blank", (c: Char) => (!isBlank(c))).+ <~ "]"
  }
  
  def rawLine: Parser[String] = {
    not(blankLine) ~> not(skipNonindentSpaces ~ noteMarker) ~> 
        indentSpaces.? ~> anyLine
  }
  
  def rawLines: Parser[String] = anyLine ~ rawLine.* ^^ {
    case first ~ rest => (first :: rest).mkString("\n")
  }
  
  def noteBlock: Parser[String] = {
    for {
      startPos <- getPosition <~ skipNonindentSpaces
      ref <- noteMarker <~ ":" <~ blankLine.? <~ indentSpaces.?
      raw <- repsep(rawLines, blankLine ~ indentSpaces ~ not(blankLine)) <~ blankLines.?
      endPos <- getPosition
      newNote = (ref, raw.mkString("\n") + "\n\n")
      st <- getState
      oldNotes = st.notes
      _ <- setState(st.copy(notes = newNote :: oldNotes))
    } yield "\n" * (endPos.line - startPos.line)
  }
  
  def parseBlocks: Parser[List[Block]] = block.* <~ eof
  
  def block: Parser[Block] = {
    val strictParsers: List[Parser[Block]] = 
        List(header, codeBlockIndented, blockQuote, hrule, bulletList,
             orderedList, htmlBlock, para, plain, emptyBlock)
    val nonStrictParsers: List[Parser[Block]] =
        List(codeBlockDelimited, macro, header, table, codeBlockIndented,
             lhsCodeBlock, blockQuote, hrule, bulletList, orderedList,
             definitionList, rawTeXBlock, para, rawHtmlBlocks, plain, emptyBlock)
    for {
      st <- getState
      blk <- if (st.strict) choice(strictParsers).named("block")
             else choice(nonStrictParsers).named("block")
    } yield blk
  }
  
  def header: Parser[Block] = (setextHeader | atxHeader).named("header")
  
  def atxHeader: Parser[Block] = {
    for {
      level <- (elem('#').+ ^^ ((ps: List[Char]) => ps.length)) <~ not(elem('.') | elem(')')) <~ skipSpaces
      text <- (inline.* <~ atxClosing) ^^ ((ins: List[Inline]) => normalizeSpaces(ins))
    } yield Header(level, text)
  }
    
  def atxClosing: Parser[String] = elem('#').* ~> blankLines
  
  def setextHeader: Parser[Block] = {
    for {
      _ <- guard(anyLine ~ elem("setextHChars", (c: Char) => setextHChars.contains(c)).+ ~ blankLine)
      text <- inline.+ <~ "\n"
      underlineChar <- elem("setextHChar", (c: Char) => setextHChars.contains(c))
      _ <- elem(underlineChar).* ~ blankLines
      level = max(0, setextHChars.indexOf(underlineChar)) + 1
    } yield Header(level, normalizeSpaces(text))
  }
  
  def hrule: Parser[Block] = {
    for {
      start <- skipSpaces ~> elem("HRuleChar", (c: Char) => isHruleChar(c))
      _ <- skipSpaces ~ elem(start) ~ skipSpaces ~ elem(start) ~ 
               (spaceChar | elem(start)).* ~ "\n" ~ blankLines.?
    } yield HorizontalRule
  }
  
  def indentedLine: Parser[String] = indentSpaces ~> """[^\n]*\n""".r
  
  def blockDelimiter(pred: Char => Boolean, len: Option[Int]): Parser[(Int, Attr, Char)] = {
    for {
      c <- guard(elem("delim", pred))
      size <- len match {
        case Some(l) => (count(l, elem(c)) ~ elem(c).*) ^^^ l
        case None => count(3, elem(c)) ~> elem(c).* ^^ ((cs: List[Char]) => 3 + cs.length)
      }
      _ <- spaceChar.*
      attr <- attributes | alphaNumStar ^^ ((s: String) => Attr("", List(s), Nil)) | success(Attr("", Nil, Nil))
      _ <- blankLine
    } yield (size, attr, c)
  }
  
  def attributes: Parser[Attr] = {
    for {
      attrs <- "{" ~> spnl ~> (attribute <~ spnl).* <~ "}"
      (ids, classes, keyvals) = (attrs.map(_.id), attrs.map(_.classes), attrs.map(_.attrs))
    } yield Attr(ids.reverse.find(_ != "").getOrElse(""), classes.flatten, keyvals.flatten)
  }
  
  def attribute: Parser[Attr] = {
    identifierAttr | classAttr | keyValAttr
  }
  
  def identifier: Parser[String] = {
    (elem("letter", (c: Char) => c.isLetter) ~ 
        elem("id", (c: Char) => c.isLetterOrDigit || "-_:.".contains(c)).*) ^^ {
      case first ~ rest => (first :: rest).mkString
    }
  }
  
  def identifierAttr: Parser[Attr] = {
    ("#" ~> identifier) ^^ ((res: String) => Attr(res, Nil, Nil))
  }
  
  def classAttr: Parser[Attr] = {
    ("." ~> identifier) ^^ ((res: String) => Attr("", List(res), Nil))
  }
  
  def keyValAttr: Parser[Attr] = {
    for {
      key <- identifier
      _ <- elem('=')
      value <- listOfCharToString(
                enclosed('"', '"', elem("any", (c: Char) => c != '"')) | 
                enclosed('\'', '\'', elem("any", (c: Char) => c != '\'')) | 
                nonspaceChar.*)
    } yield Attr("", Nil, List(KeyValue(key, value)))
  }
  
  def codeBlockDelimited: Parser[Block] = {
    for {
      sizeAttrAndC <- blockDelimiter((c: Char) => "~`".contains(c), None)
      (size, attr, c) = sizeAttrAndC
      contents <- anyLine <~ blockDelimiter((ch: Char) => ch == c, Some(size)) <~ blankLines
    } yield CodeBlock(attr, contents.mkString("\n"))
  }
  
  def codeBlockIndented: Parser[Block] = {
    def blanksPlusIndented: Parser[String] = {
      blankLines ~ indentedLine ^^ {
        case b ~ l => b + l
      }
    }
    for {
      contents <- (indentedLine | blanksPlusIndented).+ <~ blankLines.?
      st <- getState
    } yield CodeBlock(Attr("", st.indentedCodeClasses, Nil), stripTrailingNewlines(contents.mkString))
  }
  
  def lhsCodeBlock: Parser[Block] = {
    failUnlessLhs ~> 
        ((lhsCodeBlockBird | lhsCodeBlockLaTeX) ^^ (
             (s: String) => CodeBlock(Attr("", List("sourceCode", "literate", "haskell"), Nil), s)) |
         lhsCodeBlockInverseBird ^^ (
             (s: String) => CodeBlock(Attr("", List("sourceCode", "haskell"), Nil), s)))
  }
  
  def lhsCodeBlockLaTeX: Parser[String] = {
    for {
      _ <- "\\begin{code}" ~ spaceChar.* ~ "\n"
      contents <- """.+\\end{code}""" ^^ ((s: String) => s.substring(0, s.length - "\\end{code}".length))
      _ <- blankLines
    } yield stripTrailingNewlines(contents)
  }
  
  def lhsCodeBlockBird: Parser[String] = lhsCodeBlockBirdWith('>')
  
  def lhsCodeBlockInverseBird: Parser[String] = lhsCodeBlockBirdWith('<')
  
  def lhsCodeBlockBirdWith(c: Char): Parser[String] = {
    for {
      pos <- getPosition
      _ <- if (pos.column != 1) failure("Not in first column") else success(())
      lns <- birdTrackLine(c).+
      lnsPrime = if (lns.forall((s: String) => s == "" | s.substring(0, 1) == " ")) lns.map(_.drop(1)) else lns
      _ <- blankLines
    } yield lnsPrime.mkString("\n")
  }
  
  def birdTrackLine(c: Char): Parser[String] = {
    elem(c) ~> (if (c == '<') not(letter) else success(())) ~> anyLine
  }
  
  def emailBlockQuoteStart: Parser[Char] = {
    skipNonindentSpaces ~> '>' <~ opt(' ')
  }
  
  def emailBlockQuote: Parser[List[String]] = {
    val emailLine: Parser[String] = {
      (nonEndline | (endline ~ not(emailBlockQuoteStare) ^^^ '\n')).*
    }
    for {
      raw <- emailBlockQuoteStart ~> repsep(emailLine, '\n' ~ emailBlockQuoteStart)
      _ <- ('\n' | eof) ~ blankLines.?
    } yield raw
  }
    
  def blockQuote: Parser[Block] = {
    for {
      raw <- emailBlockQuote
      contents <- parseFromString(parseBlocks, raw.mkString("\n") + "\n\n")
    } yield BlockQuote(contents)
  }  
  
  def bulletListStart: Parser[Unit] = {
    ("\n".? ~ skipNonindentSpaces ~ not(hrule) ~ 
        elem("bullet list marker", isBulletListMarker(_)) ~ spaceChar ~ skipSpaces) ^^^ ()
  }
  
  def anyOrderedListStart: Parser[ListAttributes] = {
    def strictParser: Parser[ListAttributes] = {
      (digit.+ ~ "." ~ spaceChar) ^^^ ListAttributes(1, DefaultStyle, DefaultDelim)
    }
    def nonstrictParser: Parser[ListAttributes] = {
      for {
        attrs <- anyOrderedListMarker
        (num, style, delim) = (attrs.num, attrs.style, attrs.delim)
        readNext = if (delim == Period && 
              (style == UpperAlpha || (style == UpperRoman && 
               List(1, 5, 10, 50, 100, 500, 1000).contains(num)))) {
            "\t" | (" " ~ spaceChar)
          } else { spaceChar }
        _ <- readNext ~ skipSpaces
      } yield attrs
    }
    for {
      _ <- "\n".? ~ skipNonindentSpaces ~ not("p." ~ spaceChar ~ digit)
      state <- getState
      res <- if (state.strict) strictParser else nonstrictParser
    } yield res
  }
  
  def listStart: Parser[Unit] = bulletListStart | anyOrderedListStart ^^^ ()
  
  def listLine: Parser[String] = {
    for {
      _ <- not(listStart) ~ not(blankLine) ~ not(indentSpaces ~ spaceChar.* ~ listStart)
      chunks <- ("""<!--.*?-->""".r | """[^\n]""".r).* <~ "\n"
    } yield chunks.mkString + "\n"
  }
  
  def rawListItem[A](start: Parser[A]): Parser[String] = {
    for {
      _ <- start
      result <- listLine.+
      blanks <- blankLine.*
    } yield (result ++ blanks).mkString
  }
  
  def listContinuation: Parser[String] = {
    for {
      _ <- guard(indentSpaces)
      result <- listContinuationLine.+
      blanks <- blankLine.*
    } yield (result ++ blanks).mkString
  }
  
  def listContinuationLine: Parser[String] = {
    not(blankLine) ~> not(listStart) ~> indentSpaces.? ~> """[^\n]*\n""".r
  }
  
  def listItem[A](start: Parser[A]): Parser[List[Block]] = {
    for {
      first <- rawListItem(start)
      continuations <- listContinuation.*
      state <- getState
      oldContext = state.context.list
      _ <- setState(state.copy(context=state.context.copy(list=ListItemState)))
      raw = (first :: continuations).mkString
      contents <- parseFromString(parseBlocks, raw)
      _ <- updateState((st: ParserState) => st.copy(context=st.context.copy(list=oldContext)))
    } yield contents
  }
  
  def orderedList: Parser[Block] = {
    for {
      listAttrs <- guard(anyOrderedListStart)
      items <- listItem("\n".? ~> skipNonindentSpaces ~> orderedListMarker(listAttrs.style, listAttrs.delim)).+
    } yield OrderedList(listAttrs, compactify(items))
  }
  
  def bulletList: Parser[Block] = {
    listItem(bulletListStart).+ ^^ ((blks: List[List[Block]]) => BulletList(compactify(blks)))
  }
  
  def defListMarker: Parser[Unit] = {
    for {
      sps <- nonindentSpaces <~ (elem(':') | elem('~'))
      st <- getState
      tabStop = st.tabStop
      remaining = tabStop - (sps.length + 1)
      _ <- if (remaining > 0) ((" " * remaining) | "\t")
           else failure("")
    } yield ()
  }
  
  def definitionListItem: Parser[(List[Inline], List[List[Block]])] = {
    for {
      _ <- guard(anyLine ~ blankLine.? ~ defListMarker)
      term <- inline.* <~ "\n" <~ blankLine.?
      raw <- defRawBlock.+
      state <- getState
      oldContext = state.context.list
      contents <- //TODO: how do I mapM?
      _ <- updateState((st: ParserState) => st.copy(context=st.context.copy(list=oldContext)))
    } yield ((normalizeSpaces(term), contents))
  }
  
  def defRawBlock: Parser[String] = {
    def indentedLine: Parser[String] = indentSpaces ~ anyLine ^^ {
      case sps ~ ln => sps + ln
    }
    def contParser: Parser[String] = {
      for {
        lns <- (not(blankLine) ~> indentedLine).+
        trl <- blankLines | success("")
      } yield lns.mkString("\n") + trl
    }
    for {
      _ <- defListMarker
      firstLine <- anyLine
      rawLines <- (not(blankLine) ~> indentedLine).*
      trailing <- blankLines | success("")
      cont <- contParser.* ^^ ((strs: List[String]) => strs.mkString)
    } yield firstLine + "\n" + rawLines.mkString("\n") + trailing + cont
  }
  
  def inline: Parser[Inline] = whitespace
  
  def spnl: Parser[Unit] = (skipSpaces ~ "\n".? ~ skipSpaces ~ not("\n")) ^^^ ()
  
  def escapedCharPrime: Parser[Char] = {
    for {
      _ <- elem('\\')
      state <- getState
      escParser = if (state.strict) {
        elem("non alpha-numeric", (c: Char) => "\\`*_{}[]()>#+-.!~".contains(c))
      } else {
        elem("non alpha-numeric", (c: Char) => !isAlphaNumChar(c))
      }
      ch <- escParser
    } yield ch
  }
  
  def escapedChar: Parser[Inline] = {
    escapedCharPrime ^^ {
      case ' ' => Str("\u00A0")
      case '\n' => LineBreak
      case x => Str(x.toString)
    }
  }
}