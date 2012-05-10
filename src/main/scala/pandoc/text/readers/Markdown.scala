package pandoc.text.readers

import scala.util.parsing.input.CharSequenceReader

import pandoc.text._
import pandoc.text.Shared.normalizeSpaces
import pandoc.text.Definition._

object Markdown extends Parsing {
  
  def readMarkdown(state: ParserState, doc: String): Pandoc = {
    parse(parseMarkdown, new StatefulReader(state, new CharSequenceReader(doc + "\n\n")))
  }
  
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
    for {
      _ <- updateState((s: ParserState) => s.copy(context = s.context.copy(raw = true)))
      startPos <- getPosition
      st <- getState
      firstPassParser = referenceKey | (if (st.strict) pzero else noteBlock) | lineClump
      docMinusKeys <- (firstPassParser.* <~ eof) ^^ // TODO: concat
      _ <- setInput(docMinusKeys)
      _ <- setPosition(startPos)
      stPrime <- getState
      reversedNotes = stPrime.notes
    } yield { if (examples.isEmpty) doc else bottomUp(handleExampleRef)(doc)
  }
    
  def inline: Parser[Inline] = whitespace
  
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