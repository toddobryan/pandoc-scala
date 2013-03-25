package text.pandoc.writers

import scala.xml._

import org.scalatest.FunSuite

import scalaz._
import Scalaz._

class HtmlTest extends FunSuite {
  import HtmlWriter._
  import text.pandoc.definition._
import text.pandoc.Shared.WriterOptions
  
  def getStream(state: State[WriterState, NodeSeq]): NodeSeq = state.!(WriterState())
  def getStream(stateNeedingOpts: (WriterOptions => State[WriterState, NodeSeq])): NodeSeq = {
    getStream(stateNeedingOpts(WriterOptions()))
  }
  
  test("write out inlines") {
    assert(getStream(inlineToHtml(_)(Str("abc"))) === Text("abc"))
    assert(getStream(inlineToHtml(_)(Space)) === Text(" "))
    assert(getStream(inlineToHtml(_)(LineBreak)) === <br/>)
  }
}