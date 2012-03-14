package pandoc

import pandoc.util.Reader.{getParser, listParser, labelWithContent, literal, parsers, regex, Parser, ~}

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
  
  type KeyValue = (String, String)
  type Attr = (String, List[String], List[KeyValue])
  val NullAttr: Attr = ("", Nil, Nil)
    manifest[KeyValue] -> (() => dupleParser(getParser[String], getParser[String]))
    manifest[Attr] -> (() => tripleParser(getParser[String], listParser[String], listParser[KeyValue]))
  
  type TableCell = List[Block]
  type TableCellList = List[TableCell]
  
  type Format = String
  
  type BlockList = List[Block]
  type DefnItem = (List[Inline], List[List[Block]])

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
  
  def initializeParsers() {
    parsers += (
      manifest[Plain] -> (() => (regex("""Plain\s+"""r) ~> listParser[Inline]).map(Plain(_))),
      manifest[Para] -> (() => (regex("""Para\s+"""r) ~> listParser[Inline]).map(Para(_))),
      manifest[CodeBlock] -> (() => (regex("""CodeBlock\s+"""r) ~> getParser[Attr] ~ getParser[String]).map {
        case attr ~ str => CodeBlock(attr, str)
      }),
      manifest[RawBlock] -> (() => (regex("""RawBlock\s+"""r) ~> getParser[Format] ~ getParser[String]).map {
        case format ~ str => RawBlock(format, str)
      }),
      manifest[BlockQuote] -> (() => (regex("""BlockQuote\s+"""r) ~> listParser[Block]).map(BlockQuote(_))),
      manifest[BlockList] -> (() => listParser[Block]),
      manifest[OrderedList] -> (() => (regex("""OrderedList\s+"""r) ~> getParser[ListAttributes] ~ listParser[BlockList]).map {
        case attrs ~ items => OrderedList(attrs, items)
      }),
      manifest[BulletList] -> (() => (regex("""BulletList\s+"""r) ~> listParser[BlockList]).map(BulletList(_))),
      manifest[DefnItem] -> (() => 
        ((regex("""\(\s*"""r) ~> listParser[Inline] <~ regex("""\s*,\s*"""r)) ~ 
         (listParser[BlockList] <~ regex("""\s*\)"""r))).map {
          case term ~ defn => (term, defn)
        }),
      manifest[DefinitionList] -> (() => (regex("""DefinitionList\s+"""r) ~> listParser[DefnItem]).map(DefinitionList(_))),
      manifest[Header] -> (() => (regex("""Header\s+"""r) ~> getParser[Int] ~ listParser[Inline]).map{
        case level ~ content => Header(level, content)
      }),
      manifest[TableCellList] => (() => listParser[Block]),
      manifest[Table] -> (() => (regex("""Table\s+"""r) ~> listParser[Inline] ~ listParser[Alignment] ~ listParser[Double] ~
        listParser[TableCell] ~ listParser[TableCellList]).map {
          case caption ~ alignments ~ widths ~ headers ~ rows => Table(caption, alignments, widths, headers, rows)
      }),
      manifest[Block] -> (
        getParser[Plain] |
        getParser[Para] |
        getParser[CodeBlock] |
        getParser[RawBlock] |
        getParser[BlockQuote] |
        getParser[OrderedList] |
        getParser[BulletList] |
        getParser[DefinitionList] |
        getParser[Header] |
        literal("HorizantalRule").map((s) => HorizontalRule) |
        getParser[Table] |
        literal("EmptyBlock").map((s) => EmptyBlock)
    )

    manifest[QuoteType] -> (() => 
        (literal("SingleQuote").map((s) => SingleQuote) |   
         literal("DoubleQuote").map((s) => DoubleQuote))),
      
      manifest[Target] -> (() => 
        ((regex("""\(\s*"""r) ~> getParser[String]) ~ 
            (regex("""\s*,\s*"""r) ~> getParser[String] <~ regex("""\s*\)"""r))).map {
              case url ~ title => (url, title)
            }),

      manifest[MathType] -> (() =>
        (literal("DisplayMath").map((s) => DisplayMath) | 
         literal("InlineMath").map((s) => InlineMath))),
         
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
      manifest[Code] -> (() => (regex("""Code\s+"""r) ~> getParser[Attr] ~ getParser[String]).map {
        case attr ~ str => Code(attr, str) 
      }),
      manifest[Math] -> (() => (regex("""Math\s+"""r) ~> getParser[MathType] ~ getParser[String]).map {
        case kind ~ str => Math(kind, str)
      }),
  	  manifest[RawInline] -> (() => (regex("""RawInline\s+"""r) ~> getParser[Format] ~ getParser[String]).map {
  	    case format ~ str => RawInline(format, str)
  	  }),
  	  manifest[Link] -> (() => (regex("""Link\s+"""r) ~> listParser[Inline] ~ getParser[Target]).map {
  	    case linkStr ~ target => Link(linkStr, target)
  	  }),
  	  manifest[Image] -> (() => (regex("""Image\s+"""r) ~> listParser[Inline] ~ getParser[Target]).map {
  	    case altStr ~ target => Image(altStr, target)
  	  }),
  	  manifest[Note] -> (() => (regex("""Note\s+"""r) ~> listParser[Block]).map(Note(_))),
      
      manifest[Inline] -> (() => 
        getParser[Str] |
        getParser[Emph] |
        getParser[Strong] |
        getParser[Strikeout] |
        getParser[Superscript] |
        getParser[Subscript] |
        getParser[SmallCaps] |
        getParser[Quoted] |
        getParser[Cite] |
        getParser[Code] |
        literal("Space").map((s) => Space) |
        literal("LineBreak").map((s) => LineBreak) |
        getParser[Math] |
        getParser[RawInline] |
        getParser[Link] |
        getParser[Image] |
        getParser[Note]
        ),
        
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

