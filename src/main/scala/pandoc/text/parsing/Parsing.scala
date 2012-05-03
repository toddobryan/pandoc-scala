package pandoc.text.parsing

import pandoc.text.Inline
import pandoc.text.Target

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
  def toKey(inline: List[Inline]): Key = {
    
  }
}

