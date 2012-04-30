package genparser

import org.scalatest.FunSuite

class ParseTest extends FunSuite {
  val cParser: Parser[Char, Null, Char] = new LiteralParser[Char, Null]('c')
  val newlineParser: Parser[Char, Null, Char] = new LiteralParser[Char, Null]('\n')
  val xParser: Parser[Char, Null, Char] = new LiteralParser[Char, Null]('x')
  
  test("(Char)Reader") {
    val r = CharReader("abcdefgh", Pos(1, 1))
    expect('a') { r.first }
    expect(CharReader("bcdefgh", Pos(1, 2))) { r.rest }
    expect(CharReader("defgh", Pos(1, 4))) { r.drop(3) }
    expect(false) { r.rest == CharReader("", Pos(1, 2)) }
    expect(false) { r.rest == CharReader("bcdefgh", Pos(1, 1)) }
    expect(false) { r.atEnd }
    expect(true) { CharReader("", Pos(3, 5)).atEnd }
    expect(r) { r.drop(-3) }
    expect(false) { r == "blah" }
  }
  
  test("literal parser") {
    assert(cParser(null, CharReader("char", Pos(1, 1))) === Ok('c', null, CharReader("har", Pos(1, 2))))
    assert(newlineParser(null, CharReader("\n", Pos(1, 1))) === Ok('\n', null, CharReader("", Pos(2, 1))))
    assert(xParser(null, CharReader("xyz", Pos(3, 27))) === Ok('x', null, CharReader("yz", Pos(3, 28))))
    assert(xParser(null, CharReader("abc", Pos(1, 1))) === Error(null, CharReader("abc", Pos(1, 1)),
                                                           List("expected 'x', but found 'a'")))
  }
  
  test("success") {
    assert(new Success("blah")(null, CharReader("whatever", Pos(1, 1))) === 
    		Ok("blah", null, CharReader("whatever", Pos(1, 1))))
  }
  
  test("| parser") {
    val cOrX = cParser | xParser
    assert(cOrX(null, CharReader("char", Pos(1, 1))) === Ok('c', null, CharReader("har", Pos(1, 2))))
    assert(cOrX(null, CharReader("xyz", Pos(5, 3))) === Ok('x', null, CharReader("yz", Pos(5, 4))))
    assert(cOrX(null, CharReader("abc", Pos(2, 7))) === 
    		Error(null, CharReader("abc", Pos(2, 7)), "expected 'x', but found 'a'"))
  }
  
  test("? parser") { // doesn't read ahead if it fails
    assert(cParser.?(null, CharReader("char", Pos(1, 1))) === Ok(Some('c'), null, CharReader("har", Pos(1, 2))))
    assert(cParser.?(null, CharReader("abc", Pos(2, 5))) === Ok(None, null, CharReader("abc", Pos(2, 5))))
  }
  
  test("<~> parser") {
    val cThenX = cParser <~> xParser
    assert(cThenX(null, CharReader("cxdg", Pos(1, 1))) === Ok(<~>('c', 'x'), null, CharReader("dg", Pos(1, 3))))
    assert(cThenX(null, CharReader("dgcx", Pos(1, 1))) === 
    		Error(null, CharReader("dgcx", Pos(1, 1)), "expected 'c', but found 'd'"))
    assert(cThenX(null, CharReader("cdgx", Pos(1, 1))) === 
      		Error(null, CharReader("dgx", Pos(1, 2)), "expected 'x', but found 'd'"))
  }
  
  test("<~ parser") {
    val cThenX = cParser <~ xParser
    assert(cThenX(null, CharReader("cxdg", Pos(1, 1))) === Ok('c', null, CharReader("dg", Pos(1, 3))))
    assert(cThenX(null, CharReader("dgcx", Pos(1, 1))) === 
    		Error(null, CharReader("dgcx", Pos(1, 1)), "expected 'c', but found 'd'"))
    assert(cThenX(null, CharReader("cdgx", Pos(1, 1))) === 
      		Error(null, CharReader("dgx", Pos(1, 2)), "expected 'x', but found 'd'"))
  }
  
  test("~> parser") {
    val cThenX = cParser ~> xParser
    assert(cThenX(null, CharReader("cxdg", Pos(1, 1))) === Ok('x', null, CharReader("dg", Pos(1, 3))))
    assert(cThenX(null, CharReader("dgcx", Pos(1, 1))) === 
    		Error(null, CharReader("dgcx", Pos(1, 1)), "expected 'c', but found 'd'"))
    assert(cThenX(null, CharReader("cdgx", Pos(1, 1))) === 
      		Error(null, CharReader("dgx", Pos(1, 2)), "expected 'x', but found 'd'"))
  }

  test("* parser") {
    assert(cParser.*(null, CharReader("cccchar", Pos(1, 1))) === 
    		Ok(List('c', 'c', 'c', 'c'), null, CharReader("har", Pos(1, 5))))
    assert(cParser.*(null, CharReader("xyz", Pos(1, 1))) ===
            Ok(Nil, null, CharReader("xyz", Pos(1, 1))))
    expect(Error(null, CharReader("xyz", Pos(1, 1)), "the repeat combinator was applied to a parser that consumes no input")) {
      new Success("blah").*(null, CharReader("xyz", Pos(1, 1)))
    }
  }
  
  test("repsep parser") {
    val xOrYOrZ = new LiteralParser[Char, Null]('x') | new LiteralParser[Char, Null]('y') | new LiteralParser[Char, Null]('z')
    val withCommas = xOrYOrZ.repsep(new LiteralParser(','))
    assert(withCommas(null, CharReader("x,y,y,z,q", Pos(1, 1))) ===
      		Ok(List('x', 'y', 'y', 'z'), null, CharReader(",q", Pos(1, 8))))
  }
  
  test("list parser") {
    val p = new ListParser[Char, Null](List('a', 'b', 'c', 'd'))
    assert(p(null, CharReader("abcdefg", Pos(1, 1))) === Ok(List('a', 'b', 'c', 'd'), null, CharReader("efg", Pos(1, 5))))
    assert(p(null, CharReader("abcba", Pos(3, 7))) === 
    		Error(null, CharReader("ba", Pos(3, 10)), "expected 'd', but found 'b'"))
  }
  
  test("+ parser") {
    assert(cParser.+(null, CharReader("cccchar", Pos(1, 1))) === 
    		Ok(List('c', 'c', 'c', 'c'), null, CharReader("har", Pos(1, 5))))
    assert(cParser.+(null, CharReader("xyz", Pos(1, 1))) ===
            Error(null, CharReader("xyz", Pos(1, 1)), "expected 'c', but found 'x'"))
    
  }
}