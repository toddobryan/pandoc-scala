package pandoc

import pandoc.util.Reader.{getParser, listParser, literal, parsers, regex, Parser, ~}

package object text {
  case class Pandoc(meta: Meta, content: List[Block])

  case class Meta(title: List[Inline], authors: List[List[Inline]], date: List[Inline])

  sealed abstract class Alignment
  case object AlignLeft extends Alignment
  case object AlignRight extends Alignment
  case object AlignCenter extends Alignment
  case object AlignDefault extends Alignment
  
  type ListAttributes = (Int, ListNumberStyle, ListNumberDelim)

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
  
  type Attr = (String, List[String], List[(String, String)])
  val NullAttr: Attr = ("", Nil, Nil)
  
  type TableCell = List[Block]
  
  type Format = String

  sealed abstract class Block
  case class Plain(content: List[Inline]) extends Block
  case class Para(content: List[Inline]) extends Block
  case class CodeBlock(attr: Attr, str: String) extends Block
  case class RawBlock(format: Format, str: String) extends Block
  case class BlockQuote(content: List[Block]) extends Block
  case class OrderedList(attrs: ListAttributes, items: List[List[Block]]) extends Block
  case class BulletList(items: List[List[Block]]) extends Block
  case class DefinitionList(items: List[(List[Inline], List[List[Block]])]) extends Block
  case class Header(level: Int, content: List[Inline]) extends Block
  case object HorizontalRule
  case class Table(caption: List[Inline], alignments: List[Alignment], widths: List[Double], 
                   headers: List[TableCell], rows: List[List[TableCell]]) extends Block
  case object EmptyBlock

  sealed abstract class QuoteType
  case object SingleQuote extends QuoteType
  case object DoubleQuote extends QuoteType

  type Target = (String, String)

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
  
  def labelWithContent[T, U](label: String, resultType: Function1[List[T], U])(implicit man: Manifest[T]):
      (() => Parser[U]) = {
    (() => (regex("""%s\s+""".format(label).r) ~> listParser[T]).map(resultType.apply(_)))
  }
  
  def initializeParsers() {
    parsers += (
      manifest[QuoteType] -> (() => 
        (literal("SingleQuote").map((s) => SingleQuote) |   
         literal("DoubleQuote").map((s) => DoubleQuote))),
         
      manifest[Str] -> (() => (regex("""Str\s+"""r) ~> getParser[String]).map(Str(_))),
      manifest[Emph] -> labelWithContent[Inline, Emph]("Emph", Emph(_)),
      manifest[Strong] -> labelWithContent[Inline, Strong]("Strong", Strong(_)),
      manifest[Strikeout] -> labelWithContent[Inline, Strikeout]("Strikeout", Strikeout(_)),
      manifest[Superscript] -> labelWithContent[Inline, Superscript]("Superscript", Superscript(_)),
      manifest[Subscript] -> labelWithContent[Inline, Subscript]("Subscript", Subscript(_)),
      manifest[SmallCaps] -> labelWithContent[Inline, SmallCaps]("SmallCaps", SmallCaps(_)),
      manifest[Quoted] -> (() => (regex("""Quoted\s+"""r) ~> getParser[QuoteType] ~ listParser[Inline]).map {
        case kind ~ content => Quoted(kind, content)
      }),
      manifest[Cite] -> (() => (regex("""Cite\s+"""r) ~> listParser[Citation] ~ listParser[Inline]).map {
        case citations ~ content => Cite(citations, content)
      }),
      manifest[Code] -> (() => (regex("""Code\s+"""r) ~> getParser[Attr] ~ getParser[String]). map {
        case attr ~ str => Code(attr, str) 
      }),
      manifest[Space] -> (() => (regex("""Space""").map((s) => Space))),
      
      manifest[Inline] -> (() => 
        getParser[Str] |
        getParser[Emph] |
        getParser[Strong] |
        getParser[Strikeout] |
        getParser[Superscript] |
        getParser[Subscript] |
        getParser[SmallCaps] |
        getParser[Quoted] |
        getParser[Cite]),
        
      manifest[Citation] -> (() => (regex("""Citation\s+"""r) ~> getParser[String] ~ listParser[Inline] 
          ~ listParser[Inline] ~ getParser[CitationMode] ~ getParser[Int] ~ getParser[Int]).map {
            case id ~ prefix ~ suffix ~ mode ~ noteNum ~ hash => Citation(id, prefix, suffix, mode, noteNum, hash) 
          }),
      
      manifest[CitationMode] -> (() =>
        (literal("AuthorInText").map((s) => AuthorInText) | 
         literal("SuppressAuthor").map((s) => SuppressAuthor) | 
         literal("NormalCitation").map((s) => NormalCitation)))
    )
  }
}

