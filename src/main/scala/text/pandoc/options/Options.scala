package text.pandoc.options

import java.io.File

import text.csl.{ Reference, Style }
import text.highlighting.kate.Types

sealed abstract class Extension
case object Footnotes extends Extension
case object InlineNotes extends Extension
case object PandocTitleBlock extends Extension
case object MmdTitleBlock extends Extension
case object TableCaptions extends Extension
case object ImplicitFigures extends Extension
case object SimpleTables extends Extension
case object MultilineTables extends Extension
case object GridTables extends Extension
case object PipeTables extends Extension
case object Citations extends Extension
case object RawTex extends Extension
case object RawHtml extends Extension
case object TexMathDollars extends Extension
case object TexMathSingleBackslash extends Extension
case object TexMathDoubleBackslash extends Extension
case object LatexMacros extends Extension
case object FencedCodeBlocks extends Extension
case object FencedCodeAttributes extends Extension
case object BacktickCodeBlocks extends Extension
case object InlineCodeAttributes extends Extension
case object MarkdownInHtmlBlocks extends Extension
case object MarkdownAttribute extends Extension
case object EscapedLineBreaks extends Extension
case object LinkAttributes extends Extension
case object AutolinkBareUris extends Extension
case object FancyLists extends Extension
case object Startnum extends Extension
case object DefinitionLists extends Extension
case object ExampleLists extends Extension
case object AllSymbolsEscapable extends Extension
case object IntrawordUnderscores extends Extension
case object BlankBeforeBlockquote extends Extension
case object BlankBeforeHeader extends Extension
case object Strikeout extends Extension
case object Superscript extends Extension
case object Subscript extends Extension
case object HardLineBreaks extends Extension
case object LiterateHaskell extends Extension
case object Abbreviations extends Extension
case object AutoIdentifiers extends Extension
case object HeaderAttributes extends Extension
case object MmdHeaderIdentifiers extends Extension
case object ImplicitHeaderReferences extends Extension
case object LineBlocks extends Extension

object Extension {
  val pandoc: Set[Extension] = Set(
    Footnotes,
    InlineNotes,
    PandocTitleBlock,
    TableCaptions,
    ImplicitFigures,
    SimpleTables,
    MultilineTables,
    GridTables,
    PipeTables,
    Citations,
    RawTex,
    RawHtml,
    TexMathDollars,
    LatexMacros,
    FencedCodeBlocks,
    FencedCodeAttributes,
    BacktickCodeBlocks,
    InlineCodeAttributes,
    MarkdownInHtmlBlocks,
    EscapedLineBreaks,
    FancyLists,
    Startnum,
    DefinitionLists,
    ExampleLists,
    AllSymbolsEscapable,
    IntrawordUnderscores,
    BlankBeforeBlockquote,
    BlankBeforeHeader,
    Strikeout,
    Superscript,
    Subscript,
    AutoIdentifiers,
    HeaderAttributes,
    ImplicitHeaderReferences,
    LineBlocks)

  val phpMarkdownExtra = Set(
    Footnotes,
    PipeTables,
    RawHtml,
    MarkdownAttribute,
    FencedCodeBlocks,
    DefinitionLists,
    IntrawordUnderscores,
    HeaderAttributes,
    Abbreviations)

  val githubMarkdown = Set(
    PipeTables,
    RawHtml,
    TexMathSingleBackslash,
    FencedCodeBlocks,
    FencedCodeAttributes,
    BacktickCodeBlocks,
    AutolinkBareUris,
    IntrawordUnderscores,
    Strikeout,
    HardLineBreaks)

  val multimarkdown = Set(
    PipeTables,
    RawHtml,
    MarkdownAttribute,
    LinkAttributes,
    RawTex,
    TexMathDoubleBackslash,
    IntrawordUnderscores,
    MmdTitleBlock,
    Footnotes,
    DefinitionLists,
    AllSymbolsEscapable,
    ImplicitHeaderReferences,
    AutoIdentifiers,
    MmdHeaderIdentifiers)

  val strict = Set(
    RawHtml)
}

class ReaderOptions {
  val extensions: Set[Extension] = Extension.pandoc
  val smart: Boolean = false
  val strict: Boolean = false
  val standalone: Boolean = false
  val parseRaw: Boolean = false
  val columns: Int = 80
  val tabStop: Int = 4
  val oldDashes: Boolean = false
  val references: Stream[Reference] = Stream.Empty
  val citationStyle: Option[Style] = None
  val applyMacros: Boolean = true
  val indentedCodeClasses: Stream[String] = Stream.Empty
  val defaultImageExtension: String = ""
}

sealed abstract class EpubVersion
case object Epub2 extends EpubVersion
case object Epub3 extends EpubVersion

sealed abstract class HtmlMathMethod
case object PlainMath extends HtmlMathMethod
case class LaTeXMathML(url: Option[String]) extends HtmlMathMethod
case class JsMath(url: Option[String]) extends HtmlMathMethod
case object GladTeX extends HtmlMathMethod
case class WebTeX(url: String) extends HtmlMathMethod
case class MathML(url: Option[String]) extends HtmlMathMethod
case class MathJax(url: String) extends HtmlMathMethod

sealed abstract class CiteMethod
case object Citeproc extends CiteMethod
case object Natbib extends CiteMethod
case object Biblatex extends CiteMethod

sealed abstract class ObfuscationMethod
case object NoObfuscation extends ObfuscationMethod
case object ReferenceObfuscation extends ObfuscationMethod
case object JavaScriptObfuscation extends ObfuscationMethod

sealed abstract class HtmlSlideVariant
case object S5Slides extends HtmlSlideVariant
case object SlidySlides extends HtmlSlideVariant
case object DZSlides extends HtmlSlideVariant
case object NoSlides extends HtmlSlideVariant

trait WriterOptions {
  val standalone: Boolean = false
  val template: String = ""
  val variables: Stream[(String, String)] = Stream.Empty
  val tabStop: Int = 4
  val tableOfContents: Boolean = false
  val slideVariant: HtmlSlideVariant = NoSlides
  val incremental: Boolean = false
  val htmlMathMethod: HtmlMathMethod = PlainMath
  val ignoreNotes: Boolean = false
  val numberSections: Boolean = false
  val numberOffset: Stream[Int] = Stream(0, 0, 0, 0, 0, 0)
  val sectionDivs: Boolean = false
  val extensions: Set[Extension] = Extension.pandoc
  val referenceLinks: Boolean = false
  val wrapText: Boolean = true
  val columns: Int = 72
  val emailObfuscation: ObfuscationMethod = JavaScriptObfuscation
  val identifierPrefix: String = ""
  val sourceDirectory: File = new File(".")
  val userDataDir: Option[File] = None
  val citeMethod: CiteMethod = Citeproc
  val biblioFiles: Stream[File] = Stream.Empty
  val html5: Boolean = false
  val htmlQTags: Boolean = false
  val beamer: Boolean = false
  val slideLevel: Option[Int] = None
  val chapters: Boolean = false
  val listings: Boolean = false
  val highlight: Boolean = false
  val highlightStyle: Types.Style = Types.Pygments
  val setextHeaders: Boolean = true
  val texLigatures: Boolean = true
  val epubVersion: Option[EpubVersion] = None
  val epubMetadata: String = ""
  val epubStylesheet: Option[String] = None
  val epubFonts: Stream[File] = Stream.Empty
  val epubChapterLevel: Int = 1
  val tocDepth: Int = 3
  val referenceOdt: Option[File] = None
  val referenceDocx: Option[File] = None

  def isEnabled(ext: Extension): Boolean = this.extensions.contains(ext)

  def copy(
    _standalone: Boolean = this.standalone,
    _template: String = this.template,
    _variables: Stream[(String, String)] = this.variables,
    _tabStop: Int = this.tabStop,
    _tableOfContents: Boolean = this.tableOfContents,
    _slideVariant: HtmlSlideVariant = this.slideVariant,
    _incremental: Boolean = this.incremental,
    _htmlMathMethod: HtmlMathMethod = this.htmlMathMethod,
    _ignoreNotes: Boolean = this.ignoreNotes,
    _numberSections: Boolean = this.numberSections,
    _numberOffset: Stream[Int] = this.numberOffset,
    _sectionDivs: Boolean = this.sectionDivs,
    _extensions: Set[Extension] = this.extensions,
    _referenceLinks: Boolean = this.referenceLinks,
    _wrapText: Boolean = this.wrapText,
    _columns: Int = this.columns,
    _emailObfuscation: ObfuscationMethod = this.emailObfuscation,
    _identifierPrefix: String = this.identifierPrefix,
    _sourceDirectory: File = this.sourceDirectory,
    _userDataDir: Option[File] = this.userDataDir,
    _citeMethod: CiteMethod = this.citeMethod,
    _biblioFiles: Stream[File] = this.biblioFiles,
    _html5: Boolean = this.html5,
    _htmlQTags: Boolean = this.htmlQTags,
    _beamer: Boolean = this.beamer,
    _slideLevel: Option[Int] = this.slideLevel,
    _chapters: Boolean = this.chapters,
    _listings: Boolean = this.listings,
    _highlight: Boolean = this.highlight,
    _highlightStyle: Types.Style = this.highlightStyle,
    _setextHeaders: Boolean = this.setextHeaders,
    _texLigatures: Boolean = this.texLigatures,
    _epubVersion: Option[EpubVersion] = this.epubVersion,
    _epubMetadata: String = this.epubMetadata,
    _epubStylesheet: Option[String] = this.epubStylesheet,
    _epubFonts: Stream[File] = this.epubFonts,
    _epubChapterLevel: Int = this.epubChapterLevel,
    _tocDepth: Int = this.tocDepth,
    _referenceOdt: Option[File] = this.referenceOdt,
    _referenceDocx: Option[File] = this.referenceDocx) = 
  new WriterOptions {
    override val standalone = _standalone
    override val template = _template
    override val variables = _variables
    override val tabStop = _tabStop
    override val tableOfContents = _tableOfContents
    override val slideVariant = _slideVariant
    override val incremental = _incremental
    override val htmlMathMethod = _htmlMathMethod
    override val ignoreNotes = _ignoreNotes
    override val numberSections = _numberSections
    override val numberOffset = _numberOffset
    override val sectionDivs = _sectionDivs
    override val extensions = _extensions
    override val referenceLinks = _referenceLinks
    override val wrapText = _wrapText
    override val columns = _columns
    override val emailObfuscation = _emailObfuscation
    override val identifierPrefix = _identifierPrefix
    override val sourceDirectory = _sourceDirectory
    override val userDataDir = _userDataDir
    override val citeMethod = _citeMethod
    override val biblioFiles = _biblioFiles
    override val html5 = _html5
    override val htmlQTags = _htmlQTags
    override val beamer = _beamer
    override val slideLevel = _slideLevel
    override val chapters = _chapters
    override val listings = _listings
    override val highlight = _highlight
    override val highlightStyle = _highlightStyle
    override val setextHeaders = _setextHeaders
    override val texLigatures = _texLigatures
    override val epubVersion = _epubVersion
    override val epubMetadata = _epubMetadata
    override val epubStylesheet = _epubStylesheet
    override val epubFonts = _epubFonts
    override val epubChapterLevel = _epubChapterLevel
    override val tocDepth = _tocDepth
    override val referenceOdt = _referenceOdt
    override val referenceDocx = _referenceDocx
  }
}





