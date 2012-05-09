package pandoc.text

import scala.io.Source
import org.scalatest.FunSuite
import pandoc.util.Reader.{parsedPlusRest, read}
import pandoc.text.writers.Native

import Definition._
import Shared.{WriterSwitchOptions, WriterOptions}

class ParseTest extends FunSuite {
  test("parse native values") {
    assert(parsedPlusRest(read[List[Inline]]("""[Str "abc",Space,Emph [Str "emphasis"]]""")) === 
      Some(List(Str("\"abc\""), Space, Emph(List(Str("\"emphasis\"")))), ""))
  }
  
  test("read native files") {
    val htmlReaderNative = Source.fromURL(getClass.getResource("/tests/html-reader.native")).mkString
    val asNative = parsedPlusRest(read[Pandoc](htmlReaderNative))
    assert(asNative.isDefined)
    val backToNative = Native.writeNative(WriterOptions().copy(switches = WriterSwitchOptions().copy(standalone = true)), asNative.get._1)
    assert(backToNative === htmlReaderNative)
  }
}