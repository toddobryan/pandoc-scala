package pandoc.text

import java.awt.Color
import java.io.File

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
  def highlightStyle: Style = Pygments
  def setextHeaders: Boolean = true
}