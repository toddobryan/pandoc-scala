package text.pandoc.writers

import scala.xml._

import org.scalatest.FunSuite

import scalaz.State

class HtmlTest extends FunSuite {
  import HtmlWriter._
  import text.pandoc.definition._
import text.pandoc.Shared.{WriterOptions, WriterSwitchOptions}
  def htmlFromInline(inl: Inline, st: WriterState = WriterState()) = inlineToHtml(WriterOptions())(inl).eval(st).toString
    
  test("write out inlines") {
    assert(htmlFromInline(Str("abc")) === Text("abc").toString)
    assert(htmlFromInline(Space) === Text(" ").toString)
    assert(htmlFromInline(LineBreak) === <br/>.toString)
    assert(htmlFromInline(Emph(Stream(Str("abc"), Space, Str("123"), LineBreak))) === 
      <em>abc 123<br/></em>.toString)
  }
  
  test("qtags") {
    assert(htmlFromInline(Quoted(SingleQuote, Stream(Str("abc")))) === 
      Text("\u2018abc\u2019").toString)
    assert(htmlFromInline(Quoted(DoubleQuote, Stream(Str("abc")))) === 
      Text("\u201Cabc\u201D").toString)
    val (st, res) = inlineToHtml(WriterOptions(switches=WriterSwitchOptions(htmlQtags=true))
        )(Quoted(DoubleQuote, Stream(Str("abc")))).run(WriterState())
    assert(st.usesQtag === true)
    assert(res.toString === <q>abc</q>.toString)
  }
}