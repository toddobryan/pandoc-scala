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

}