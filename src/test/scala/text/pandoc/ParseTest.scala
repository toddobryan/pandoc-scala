package text.pandoc

import scala.io.Source
import org.scalatest.FunSuite
import text.pandoc.MyReader.{parsedPlusRest, read}
import text.pandoc.writers.Native

import definition._
import Shared.{WriterSwitchOptions, WriterOptions}

class ParseTest extends FunSuite {
  test("parse native values") {
    assert(parsedPlusRest(read[Stream[Inline]]("""[Str "abc",Space,Emph [Str "emphasis"]]""")) === 
      Some((Stream(Str("\"abc\""), Space, Emph(Stream(Str("\"emphasis\"")))), "")))
  }
  
  /*test("read native files") {
    val htmlReaderNative = Source.fromURL(getClass.getResource("/tests/html-reader.native")).mkString
    val asNative = parsedPlusRest(read[Pandoc](htmlReaderNative))
    assert(asNative.isDefined)
    val backToNative = Native.writeNative(WriterOptions().copy(switches = WriterSwitchOptions().copy(standalone = true)), asNative.get._1)
    assert(backToNative === htmlReaderNative)
  }*/
}