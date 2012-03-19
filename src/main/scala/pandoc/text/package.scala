package pandoc

import pandoc.util.Reader.{dupleParser, tripleParser, getParser, listParser, labelWithContent, literal, parsers, regex, Parser, ~}

package object text {
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
  val NullAttr = Attr("", Nil, Nil)
  
  type TableCell = List[Block]
  
  type Format = String
  
  type DefnItem = (List[Inline], List[List[Block]])
  
  sealed abstract class Block
  case class Plain(content: List[Inline]) extends Block
  case class Para(content: List[Inline]) extends Block
  case class CodeBlock(attr: Attr, str: String) extends Block
  case class RawBlock(format: Format, str: String) extends Block
  case class BlockQuote(content: List[Block]) extends Block
  case class OrderedList(attrs: ListAttributes, items: List[List[Block]]) extends Block
  case class BulletList(items: List[List[Block]]) extends Block
  case class DefinitionList(items: List[DefnItem]) extends Block
  case class Header(level: Int, content: List[Inline]) extends Block
  case object HorizontalRule extends Block
  case class Table(caption: List[Inline], alignments: List[Alignment], widths: List[Double], 
                   headers: List[TableCell], rows: List[List[TableCell]]) extends Block
  case object EmptyBlock extends Block
  

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
    parsers += manifest[Pandoc] -> (() => (regex("""Pandoc\s+"""r) ~> getParser[Meta] ~ getParser[List[Block]]).map {
      case meta ~ content => Pandoc(meta, content)
    })

    parsers += manifest[Meta] -> (() => 
      ((regex("""\(\s*Meta\s+\{\s*docTitle\s*=\s*"""r) ~> getParser[List[Inline]] <~ regex("""\s*,\s*docAuthors\s*=\s*"""r)) ~ 
          (getParser[List[List[Inline]]] <~ regex("""\s*,\s*docDate\s*=\s*"""r)) ~
          (getParser[List[Inline]] <~ regex("""\s*\}\s*\)"""r))).map{
      case title ~ authors ~ date => Meta(title, authors, date)  
    })

    parsers += manifest[Alignment] -> (() => 
      literal("AlignLeft").map((s) => AlignLeft) |
      literal("AlignRight").map((s) => AlignRight) |
      literal("AlignCenter").map((s) => AlignCenter) |
      literal("AlignDefault").map((s) => AlignDefault)
    )
  
    parsers += manifest[ListAttributes] -> (() => tripleParser(getParser[Int], getParser[ListNumberStyle], getParser[ListNumberDelim]).map {
      case (num, style, delim) => ListAttributes(num, style, delim)
    })

    parsers += manifest[ListNumberStyle] -> (() =>
      literal("DefaultStyle").map((s) => DefaultStyle) |
      literal("Example").map((s) => Example) |
      literal("Decimal").map((s) => Decimal) |
      literal("LowerRoman").map((s) => LowerRoman) |
      literal("UpperRoman").map((s) => UpperRoman) |
      literal("LowerAlpha").map((s) => LowerAlpha) |
      literal("UpperAlpha").map((s) => UpperAlpha)    
    )
    
    parsers += manifest[ListNumberDelim] -> (() =>
      literal("DefaultDelim").map((s) => DefaultDelim) |
      literal("Period").map((s) => Period) |
      literal("OneParen").map((s) => OneParen) |
      literal("TwoParens").map((s) => TwoParens)
    )

    parsers += manifest[KeyValue] -> (() => dupleParser(getParser[String], getParser[String]).map{
      case (key, value) => KeyValue(key, value)
    })
    parsers += manifest[Attr] -> (() => tripleParser(getParser[String], getParser[List[String]], getParser[List[KeyValue]]).map{
      case (id, classes, attrs) => Attr(id, classes, attrs)
    })
    parsers += manifest[Plain] -> (() => (regex("""Plain\s+"""r) ~> getParser[List[Inline]]).map(Plain(_)))
    parsers += manifest[Para] -> (() => (regex("""Para\s+"""r) ~> getParser[List[Inline]]).map(Para(_)))
    parsers += manifest[CodeBlock] -> (() => (regex("""CodeBlock\s+"""r) ~> getParser[Attr] ~  getParser[String]).map {
        case attr ~ str => CodeBlock(attr, str)
      })
    parsers += manifest[RawBlock] -> (() => (regex("""RawBlock\s+"""r) ~> getParser[Format] ~ getParser[String]).map {
        case format ~ str => RawBlock(format, str)
      })
    parsers += manifest[BlockQuote] -> (() => (regex("""BlockQuote\s+"""r) ~> getParser[List[Block]]).map(BlockQuote(_)))
    parsers += manifest[OrderedList] -> (() => (regex("""OrderedList\s+"""r) ~> getParser[ListAttributes] ~ getParser[List[List[Block]]]).map {
        case attrs ~ items => OrderedList(attrs, items)
      })
    parsers += manifest[BulletList] -> (() => (regex("""BulletList\s+"""r) ~> getParser[List[List[Block]]]).map(BulletList(_)))
    parsers += manifest[DefnItem] -> (() => dupleParser(getParser[List[Inline]], getParser[List[List[Block]]]))
    parsers += manifest[DefinitionList] -> (() => (regex("""DefinitionList\s+"""r) ~> getParser[List[DefnItem]]).map(DefinitionList(_)))
    parsers += manifest[Header] -> (() => (regex("""Header\s+"""r) ~> getParser[Int] ~ getParser[List[Inline]]).map{
        case level ~ content => Header(level, content)
      })
    parsers += manifest[Table] -> (() => (regex("""Table\s+"""r) ~> getParser[List[Inline]] ~ getParser[List[Alignment]] ~ getParser[List[Double]] ~
        getParser[List[TableCell]] ~ getParser[List[List[TableCell]]]).map {
          case caption ~ alignments ~ widths ~ headers ~ rows => Table(caption, alignments, widths, headers, rows)
      })
    parsers +=  manifest[Block] -> (() =>
        getParser[Plain] |
        getParser[Para] |
        getParser[CodeBlock] |
        getParser[RawBlock] |
        getParser[BlockQuote] |
        getParser[OrderedList] |
        getParser[BulletList] |
        getParser[DefinitionList] |
        getParser[Header] |
        literal("HorizontalRule").map((s) => HorizontalRule) |
        getParser[Table] |
        literal("EmptyBlock").map((s) => EmptyBlock)
      )

    parsers += manifest[QuoteType] -> (() => 
        (literal("SingleQuote").map((s) => SingleQuote) |   
         literal("DoubleQuote").map((s) => DoubleQuote)))
      
    parsers += manifest[Target] -> (() => 
        ((regex("""\(\s*"""r) ~> getParser[String]) ~ 
            (regex("""\s*,\s*"""r) ~> getParser[String] <~ regex("""\s*\)"""r))).map {
              case url ~ title => (url, title)
            })

    parsers += manifest[MathType] -> (() =>
        (literal("DisplayMath").map((s) => DisplayMath) | 
         literal("InlineMath").map((s) => InlineMath)))
         
    parsers += manifest[Str] -> (() => (regex("""Str\s+"""r) ~> getParser[String]).map(Str(_)))
    parsers += manifest[Emph] -> labelWithContent[Inline, Emph]("Emph", Emph(_))
    parsers += manifest[Strong] -> labelWithContent[Inline, Strong]("Strong", Strong(_))
    parsers += manifest[Strikeout] -> labelWithContent[Inline, Strikeout]("Strikeout", Strikeout(_))
    parsers += manifest[Superscript] -> labelWithContent[Inline, Superscript]("Superscript", Superscript(_))
    parsers += manifest[Subscript] -> labelWithContent[Inline, Subscript]("Subscript", Subscript(_))
    parsers += manifest[SmallCaps] -> labelWithContent[Inline, SmallCaps]("SmallCaps", SmallCaps(_))
    parsers += manifest[Quoted] -> (() => (regex("""Quoted\s+"""r) ~> getParser[QuoteType] ~ getParser[List[Inline]]).map {
        case kind ~ content => Quoted(kind, content)
      })
    parsers += manifest[Cite] -> (() => (regex("""Cite\s+"""r) ~> getParser[List[Citation]] ~ getParser[List[Inline]]).map {
        case citations ~ content => Cite(citations, content)
      })
    parsers += manifest[Code] -> (() => (regex("""Code\s+"""r) ~> getParser[Attr] ~ getParser[String]).map {
        case attr ~ str => Code(attr, str) 
      })
    parsers += manifest[Math] -> (() => (regex("""Math\s+"""r) ~> getParser[MathType] ~ getParser[String]).map {
        case kind ~ str => Math(kind, str)
      })
  	parsers += manifest[RawInline] -> (() => (regex("""RawInline\s+"""r) ~> getParser[Format] ~ getParser[String]).map {
  	    case format ~ str => RawInline(format, str)
  	  })
  	parsers += manifest[Link] -> (() => (regex("""Link\s+"""r) ~> getParser[List[Inline]] ~ getParser[Target]).map {
  	    case linkStr ~ target => Link(linkStr, target)
  	  })
  	parsers += manifest[Image] -> (() => (regex("""Image\s+"""r) ~> getParser[List[Inline]] ~ getParser[Target]).map {
  	    case altStr ~ target => Image(altStr, target)
  	  })
  	parsers += manifest[Note] -> (() => (regex("""Note\s+"""r) ~> getParser[List[Block]]).map(Note(_)))
      
    parsers += manifest[Inline] -> (() => 
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
      )
        
    parsers += manifest[Citation] -> (() => (regex("""Citation\s+"""r) ~> getParser[String] ~ getParser[List[Inline]] 
          ~ getParser[List[Inline]] ~ getParser[CitationMode] ~ getParser[Int] ~ getParser[Int]).map {
            case id ~ prefix ~ suffix ~ mode ~ noteNum ~ hash => Citation(id, prefix, suffix, mode, noteNum, hash) 
        })
      
    parsers += manifest[CitationMode] -> (() =>
        (literal("AuthorInText").map((s) => AuthorInText) | 
         literal("SuppressAuthor").map((s) => SuppressAuthor) | 
         literal("NormalCitation").map((s) => NormalCitation)))
  }
}

