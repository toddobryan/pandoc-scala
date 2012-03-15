package pandoc.text

import java.awt.Color
import java.io.File
import javax.swing.text.Style

sealed abstract class HtmlSlideVariant
case object S5Slides extends HtmlSlideVariant
case object SlidySlides extends HtmlSlideVariant
case object DZSlides extends HtmlSlideVariant
case object NoSlides extends HtmlSlideVariant

sealed abstract class HtmlMathMethod
case object PlainMath extends HtmlMathMethod
case class LaTeXMathML(url: Option[String]) extends HtmlMathMethod
case class JsMath(url: Option[String]) extends HtmlMathMethod
case object GladTeX extends HtmlMathMethod
case class WebTeX(url: String) extends HtmlMathMethod
case class MathML(url: Option[String]) extends HtmlMathMethod
case class MathJax(url: String) extends HtmlMathMethod

sealed abstract class ObfuscationMethod
case object NoObfuscation extends ObfuscationMethod
case object ReferenceObfuscation extends ObfuscationMethod
case object JavaScriptObfuscation extends ObfuscationMethod

sealed abstract class CiteMethod
case object Citeproc extends CiteMethod
case object Natbib extends CiteMethod
case object Biblatex extends CiteMethod

/*
sealed abstract class TokenType
case object KeywordTok extends TokenType
case object DataTypeTok extends TokenType
case object DecValTok extends TokenType
case object BaseNTok extends TokenType
case object FloatTok extends TokenType
case object CharTok extends TokenType
case object StringTok extends TokenType
case object CommentTok extends TokenType
case object OtherTok extends TokenType
case object AlertTok extends TokenType
case object FunctionTok extends TokenType
case object RegionMarkerTok extends TokenType
case object ErrorTok extends TokenType
case object NormalTok extends TokenType

case class TokenStyle(
    color: Option[Color] = None,
    background: Option[Color] = None,
    bold: Boolean = false,
    italic: Boolean = false,
    underline: Boolean = false)

case class Style(
    tokenStyles: Map[TokenType, TokenStyle], 
    defaultColor: Option[Color],
    backgroundColor: Option[Color],
    lineNumberColor: Option[Color],
    lineNumberBackgroundColor: Option[Color])

object Pygments extends Style(
    Map(KeywordTok -> TokenStyle(color = Some(new Color(0, 112, 32)), 
    					bold = true),
    	DataTypeTok -> TokenStyle(color = Some(new Color(144, 32, 0))),
    	DecValTok -> TokenStyle(color = Some(new Color(64, 160, 112))),
    	BaseNTok -> TokenStyle(color = Some(new Color(64, 160, 112))),
    	FloatTok -> TokenStyle(color = Some(new Color(64, 160, 112))),
    	CharTok -> TokenStyle(color = Some(new Color(64, 160, 112))),
    	StringTok -> TokenStyle(color = Some(new Color(64, 160, 112))),
    	CommentTok -> TokenStyle(color = Some(new Color(96, 160, 176))),
    	OtherTok -> TokenStyle(color = Some(new Color(0, 112, 32))),
    	AlertTok -> TokenStyle()
    	),
    None, None, Some(new Color(170, 170, 170)), None)
*/

trait WriterOptions {
  def standalone: Boolean = false
  def template: String = ""
  def variables: List[(String, String)] = Nil
  def epubMetadata: String = ""
  def tabStop: Int = 4
  def tableOfContents: Boolean = false
  def slideVariant: HtmlSlideVariant = NoSlides
  def incremental: Boolean = false
  def xeTeX: Boolean = false
  def htmlMathMethod: HtmlMathMethod = PlainMath
  def ignoreNotes: Boolean = false
  def numberSections: Boolean = false
  def sectionDivs: Boolean = false
  def strictMarkdown: Boolean = false
  def referenceLinks: Boolean = false
  def wrapText: Boolean = true
  def columns: Int = 72
  def literateHaskell: Boolean = false
  def emailObfuscation: ObfuscationMethod = JavaScriptObfuscation
  def identifierPrefix: String = ""
  def sourceDirectory: File = new File(".")
  def userDataDir: Option[File] = None
  def citeMethod: CiteMethod = Citeproc
  def biblioFiles: List[File] = Nil
  def html5: Boolean = false
  def beamer: Boolean = false
  def slideLevel: Option[Int] = None
  def chapters: Boolean = false
  def listings: Boolean = false
  def highlight: Boolean = false
  def highlightStyle: Style = 
  
                , writerBiblioFiles      = []
                , writerHtml5            = False
                , writerBeamer           = False
                , writerSlideLevel       = Nothing
                , writerChapters         = False
                , writerListings         = False
                , writerHighlight        = False
                , writerHighlightStyle   = pygments
                , writerSetextHeaders    = True
                }

}