package text.pandoc.definition

case class Pandoc(meta: Meta, content: Stream[Block])

case class Meta(title: Stream[Inline], authors: Stream[Stream[Inline]], date: Stream[Inline])

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
case class Attr(id: String, classes: Stream[String], attrs: Stream[KeyValue])
object NullAttr extends Attr("", Stream.Empty, Stream.Empty)

case class TableCell(wrapped: Stream[Block])

case class Format(wrapped: String)

case class DefnItem(item: Stream[Inline], defn: Stream[Stream[Block]])

sealed abstract class Block
case class Plain(content: Stream[Inline]) extends Block
case class Para(content: Stream[Inline]) extends Block
case class CodeBlock(attr: Attr, str: String) extends Block
case class RawBlock(format: Format, str: String) extends Block
case class BlockQuote(content: Stream[Block]) extends Block
case class OrderedList(attrs: ListAttributes, items: Stream[Stream[Block]]) extends Block
case class BulletList(items: Stream[Stream[Block]]) extends Block
case class DefinitionList(items: Stream[DefnItem]) extends Block
case class Header(level: Int, content: Stream[Inline]) extends Block
case object HorizontalRule extends Block
case class Table(caption: Stream[Inline], alignments: Stream[Alignment], widths: Stream[Double],
  headers: Stream[TableCell], rows: Stream[Stream[TableCell]]) extends Block
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
case class Emph(content: Stream[Inline]) extends Inline
case class Strong(content: Stream[Inline]) extends Inline
case class Strikeout(content: Stream[Inline]) extends Inline
case class Superscript(content: Stream[Inline]) extends Inline
case class Subscript(content: Stream[Inline]) extends Inline
case class SmallCaps(content: Stream[Inline]) extends Inline
case class Quoted(kind: QuoteType, content: Stream[Inline]) extends Inline
case class Cite(citations: Stream[Citation], content: Stream[Inline]) extends Inline
case class Code(attr: Attr, str: String) extends Inline
case object Space extends Inline
case object LineBreak extends Inline
case class Math(kind: MathType, str: String) extends Inline
case class RawInline(format: Format, str: String) extends Inline
case class Link(linkStr: Stream[Inline], target: Target) extends Inline
case class Image(altStr: Stream[Inline], target: Target) extends Inline
case class Note(content: Stream[Block]) extends Inline

case class Citation(id: String, prefix: Stream[Inline], suffix: Stream[Inline],
  mode: CitationMode, noteNum: Int, hash: Int)

sealed abstract class CitationMode
case object AuthorInText extends CitationMode
case object SuppressAuthor extends CitationMode
case object NormalCitation extends CitationMode