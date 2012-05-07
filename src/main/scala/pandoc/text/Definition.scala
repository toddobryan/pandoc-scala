package pandoc.text

object Definition {
  case class Pandoc(meta: Meta, content: List[Block])

  case class Meta(title: List[Inline], authors: List[List[Inline]], date: List[Inline])

  sealed abstract class Alignment
  case object AlignLeft extends Alignment
  case object AlignRight extends Alignment
  case object AlignCenter extends Alignment
  case object AlignDefault extends Alignment

  case class ListAttributes(num: Int, style: ListNumberStyle, delim: ListNumberDelim)
  
  sealed abstract class ListNumberStyle
  case object DefaultStyle extends ListNumberStyle
  case object Example extends ListNumberStyle
  case object Decimal extends ListNumberStyle
  case object LowerRoman extends ListNumberStyle
  case object UpperRoman extends ListNumberStyle
  case object LowerAlpha extends ListNumberStyle
  case object UpperAlpha extends ListNumberStyle
  
  sealed abstract class ListNumberDelim
  case object DefaultDelim extends ListNumberDelim
  case object Period extends ListNumberDelim
  case object OneParen extends ListNumberDelim
  case object TwoParens extends ListNumberDelim
  
  case class KeyValue(key: String, value: String)
  case class Attr(id: String, classes: List[String], attrs: List[KeyValue])
  object NullAttr extends Attr("", Nil, Nil)
  
  case class TableCell(wrapped: List[Block])
  
  case class Format(wrapped: String)
  
  case class DefnItem(item: List[Inline], defn: List[List[Block]])
  
  sealed abstract class Block
  case class Plain(content: List[Inline]) extends Block
  case class Para(content: List[Inline]) extends Block
  case class CodeBlock(attr: Attr, str: String) extends Block
  case class RawBlock(format: String, str: String) extends Block
  case class BlockQuote(content: List[Block]) extends Block
  case class OrderedList(attrs: ListAttributes, items: List[List[Block]]) extends Block
  case class BulletList(items: List[List[Block]]) extends Block
  case class DefinitionList(items: List[DefnItem]) extends Block
  case class Header(level: Int, content: List[Inline]) extends Block
  case object HorizontalRule extends Block
  case class Table(caption: List[Inline], alignments: List[Alignment], widths: List[Double], 
                   headers: List[TableCell], rows: List[List[TableCell]]) extends Block
  case object EmptyBlock extends Block // Null
  

  sealed abstract class QuoteType
  case object SingleQuote extends QuoteType
  case object DoubleQuote extends QuoteType

  case class Target(url: String, title: String)

  sealed abstract class MathType
  case object DisplayMath extends MathType
  case object InlineMath extends MathType

  sealed abstract class Inline
  case class Str(str: String) extends Inline
  case class Emph(content: List[Inline]) extends Inline
  case class Strong(content: List[Inline]) extends Inline
  case class Strikeout(content: List[Inline]) extends Inline
  case class Superscript(content: List[Inline]) extends Inline
  case class Subscript(content: List[Inline]) extends Inline
  case class SmallCaps(content: List[Inline]) extends Inline
  case class Quoted(kind: QuoteType, content: List[Inline]) extends Inline
  case class Cite(citations: List[Citation], content: List[Inline]) extends Inline
  case class Code(attr: Attr, str: String) extends Inline
  case object Space extends Inline
  case object LineBreak extends Inline
  case class Math(kind: MathType, str: String) extends Inline
  case class RawInline(format: Format, str: String) extends Inline
  case class Link(linkStr: List[Inline], target: Target) extends Inline
  case class Image(altStr: List[Inline], target: Target) extends Inline
  case class Note(content: List[Block])
  
  case class Citation(id: String, prefix: List[Inline], suffix: List[Inline], 
                      mode: CitationMode, noteNum: Int, hash: Int)
                    
  sealed abstract class CitationMode
  case object AuthorInText extends CitationMode 
  case object SuppressAuthor extends CitationMode
  case object NormalCitation extends CitationMode
}
