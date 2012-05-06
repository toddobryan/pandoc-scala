package pandoc.text.parsing

import shapeless.SybClass._
import shapeless.TypeOperators._
import shapeless.Poly1

import genparser._

import pandoc.text._


case class SourcePos(sourceName: String, line: Int, column: Int)

sealed abstract class HeaderType
case class SingleHeader(ch: Char) extends HeaderType
case class DoubleHeader(ch: Char) extends HeaderType

sealed abstract class ParserContext
case object ListItemState extends ParserContext
case object NullState extends ParserContext

sealed abstract class QuoteContext
case object InSingleQuote extends QuoteContext
case object InDoubleQuote extends QuoteContext
case object NoQuote extends QuoteContext

case class Key(content: List[Inline])
class KeyTable extends Map[Key, Target]

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

object InlineParsers {
  object lowercase extends Poly1 {
    implicit def caseInline = at[Inline]((in: Inline) => in match {
      case Str(s) => Str(s.toLowerCase)
      case Math(t, s) => Math(t, s.toLowerCase)
      case Code(attr, s) => Code(attr, s.toLowerCase)
      case RawInline(f, s) => RawInline(f, s.toLowerCase)
      case LineBreak => Space
      case x => x
    })
    implicit def default[T] = at[T](t => t)
  }
  
  def toKey(inline: List[Inline]): Key = {
    Key(everywhere(lowercase)(inline))
  }
  
  def fromKey(key: Key): List[Inline] = key.content
  
  def lookupKeySrc(table: KeyTable, key: Key): Option[Target] = {
    table.get(key)
  }
  
  def failUnlessSmart[Elem] = Parser[Unit, ParserState, Elem] {
    (state: ParserState, in: Reader[Elem]) => {
      if (state.smart) Ok((), state, in)
      else Error(state, in, "not in smart typography mode")
    }
  }
  
  def smartPunctuation(inlineParser: Parser[Inline, ParserState, Char]): Parser[Inline, ParserState, Char] = {
    failUnlessSmart[Char] ~> new ChoiceParser(quoted(inlineParser), apostrophe, dash, ellipses)
  }
  
  def apostrophe: Parser[Inline, ParserState, Char] = oneOf("'\u2019") ^^^ Str("\u2019")
  
  def quoted(inlineParser: Parser[Inline, ParserState, Char]): Parser[Inline, ParserState, Char] = {
    doubleQuoted(inlineParser) | singleQuoted(inlineParser)
  }
  
  def withQuoteContext(context: QuoteContext, parser: Parser[Inline, ParserState, Char]): Parser[Inline, ParserState, Char] = {
    for {
      oldState <- new State[ParserState, Char]()
      val oldQuoteContext = oldState.context.quote
      
    } yield Space
  }
}

