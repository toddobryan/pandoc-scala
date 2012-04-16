package pandoc.text.parsing
import org.scalatest.FunSuite
import pandoc.util.Reader.parsedPlusRest
import GenericParsers._

class GenericParsersTest extends FunSuite {
  def parsedPlusRest[T](res: ParseResult[T]): Option[(T, String)] = {
    res match {
      case Success(value, rest) => Some(value, rest.source.subSequence(res.next.offset, rest.source.length).toString)
      case _ => None
    }
  }

  
  implicit val s: ParserState = ParserState()
  
  test("anyLine") {
    assert(parsedPlusRest(parseAll(anyLine, "abcdef\n")) === Some("abcdef", ""))
    assert(parsedPlusRest(parse(anyLine, "abcdef\nghi\n")) === Some("abcdef", "ghi\n"))
  }
  
  test("spaceChar") {
    assert(parsedPlusRest(parse(spaceChar, "  ")) === Some(" ", " "))
    assert(parsedPlusRest(parse(spaceChar, "\t \t")) === Some("\t", " \t"))
  }
  
  test("skipSpaces") {
    assert(parsedPlusRest(parse(skipSpaces, "    abc   ")) === Some("", "abc   "))
  }
  
  test("blankLine") {
    assert(parsedPlusRest(parse(blankLine, "   \t   \t \nabc\n")) ===
      Some("\n", "abc\n"))
  }
  
  test("blankLines") {
    assert(parsedPlusRest(parse(blankLines, "   \n\t   \t\n \n\nabc\n")) ===
      Some("\n\n\n\n", "abc\n"))
  }
  
  test("enclosed") {
    assert(parsedPlusRest(parse(enclosed("[", "]", "[ab]".r), "[aba]ab")) ===
      Some(List("a", "b", "a"), "ab"))
  }
  
  test("stringAnyCase") {
    assert(parsedPlusRest(parse(stringAnyCase("doG"), "DOgCat")) ===
      Some("DOg", "Cat"))
  }
  
  test("lineClump") {
    val lines = """some lines
in a row
and a blank


so what happens"""
    assert(parsedPlusRest(parse(lineClump, lines)) === 
      Some("some lines\nin a row\nand a blank", "\n\nso what happens"))
  }

  test("romanNumeral") {
    assert(parsedPlusRest(parse(romanNumeral(false), "")) === None)
    assert(parsedPlusRest(parse(romanNumeral(false), "i")) === Some(1, ""))
    assert(parsedPlusRest(parse(romanNumeral(false), "mcmxciv")) === Some(1994, ""))
    assert(parsedPlusRest(parse(romanNumeral(false), "dcclxvi")) === Some(766, ""))
    assert(parsedPlusRest(parse(romanNumeral(true), "MMMCMXCIX")) === Some(3999, ""))
    assert(parsedPlusRest(parse(romanNumeral(true), "XCIVabc")) === Some(94, "abc"))
    assert(parsedPlusRest(parse(romanNumeral(false), "abc")) === None)
  }
    
  test("domain") {
    assert(parsedPlusRest(parse(domain, "www.beloit.edu")) === Some("www.beloit.edu", ""))
    assert(parsedPlusRest(parse(domain, "www.pen-island.com is here")) === Some("www.pen-island.com", " is here"))    
  }
  
  test("emailAddress") {
    assert(parsedPlusRest(parse(emailAddress, "todd.obryan@jefferson.kyschools.us")) ===
      Some(("todd.obryan@jefferson.kyschools.us", "mailto:todd.obryan@jefferson.kyschools.us"), ""))
  }
  
  test("withHorizDisplacement") {
    assert(parsedPlusRest(parse(withHorizDisplacement(romanNumeral(true)), "XV 2")) ===
      Some((15, 2), " 2"))
  }
  
  test("withRaw") {
    assert(parsedPlusRest(parse(withHorizDisplacement(romanNumeral(true)), "XV 2")) ===
      Some((15, "XV"), " 2"))
  }
}