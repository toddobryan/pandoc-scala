package genparser

import org.scalatest.FunSuite

class ParseTest extends FunSuite {
  val cParser: Parser[Char, Null, Char] = Parser.literal[Char, Null]('c')
  val newlineParser: Parser[Char, Null, Char] = Parser.literal[Char, Null]('\n')
  val xParser: Parser[Char, Null, Char] = Parser.literal[Char, Null]('x')
  
  test("literal parser") {
    assert(cParser(null, CharReader("char", Pos(1, 1))) === Ok('c', null, CharReader("har", Pos(1, 2))))
    assert(newlineParser(null, CharReader("\n", Pos(1, 1))) === Ok('\n', null, CharReader("", Pos(2, 1))))
    assert(xParser(null, CharReader("xyz", Pos(3, 27))) === Ok('x', null, CharReader("yz", Pos(3, 28))))
    assert(xParser(null, CharReader("abc", Pos(1, 1))) === Error(null, CharReader("bc", Pos(1, 2)),
                                                           List("expected 'x', but found 'a'")))
  }
  
  test("probe parser") { // doesn't read ahead if it fails
    assert(cParser.probe(null, CharReader("char", Pos(1, 1))) === Ok('c', null, CharReader("har", Pos(1, 2))))
    assert(cParser.probe(null, CharReader("abc", Pos(2, 5))) === Error(null, CharReader("abc", Pos(2, 5)),
                                                                       List("expected 'c', but found 'a'")))
  }
}