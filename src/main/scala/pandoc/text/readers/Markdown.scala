package pandoc.text.readers

import genparser.Parser
import pandoc.text.parsing.ParserState
import pandoc.text._

/*object Markdown {
  def indentSpaces: Parser[List[Char], ParserState, Char] = {
    
  }
  
  def nonindentSpaces(implicit state: ParserState): Parser[String] =  new Parser[String] {
    def apply(in: Input): ParseResult[String] = {
      parse("[ ]*".r, in) match {
        case Success(spaces, next) => {
          if (spaces.length < state.tabStop) {
            Success(spaces, next) 
          } else {
            Failure("unexpected indented line", in)
          }
        }
        case x => x
      }
    }
  }
  
  def skipNonindentSpaces(implicit state: ParserState): Parser[String] = {
    "[ ]{0, %d}".format(state.tabStop - 1).r ~> ""
  }
  
  def blankLine(implicit state: ParserState): Parser[String] = """[ \t]*""".r ~> "\n"
  
  def litChar(implicit state: ParserState): Parser[String] = {
    def singleNewLine: Parser[String] = new Parser[String] {
      def apply(in: Input): ParseResult[String] = {
        parse("\n" <~ not(blankLine), in) match {
          case Success(str, next) => Success(" ", next)
          case x => x
        }
      }
    }
    escapedCharPrime | """[^\n]""".r | singleNewLine 
  }
  
  def failUnlessBeginningOfLine(implicit state: ParserState): Parser[String] = new Parser[String] {
    def apply(in: Input): ParseResult[String] = {
      if (in.pos.column == 1) Success("", in)
      else Failure("not beginning of line", in)
    }
  }
/*  
  def inlinesInBalancedBrackets(inlineParser: Parser[Inline])(implicit state: ParserState): Parser[List[Inline]] = {
	def recursiveParser: Parser[Inline] = {
	  (guard("[") ~> inlinesInBalancedBrackets(inlineParser)) ^^ 
	  		((bal: List[Inline]) => List[Inline](Str("[")) ++ bal ++ List[Inline](Str("]"))) |
	  inlineParser
	}
    "[" ~> (recursiveParser.*) <~ "]"
  }
  
  def titleLine(implicit state: ParserState): Parser[List[Inline]] = {
    """%[ \t]*""".r ~ 
  }
  
  def endline(implicit state: ParserState): Parser[Inline] = {
    def conds: Parser[Inline] = {
      if (state.strict not(emailBlockQuoteStart) ~ not("#")
      val list = not(bulletListStart) ~ not(anyOrderedListStart)
      
    }
    "\n" <~ not(blankLine) <~ conds
  }
*/  
  
  def escapedCharPrime(implicit state: ParserState): Parser[String] = {
    if (state.strict) """\\[\\`*_{}\[\]()>#+-.!~]""".r
    else """\\[^a-zA-Z0-9]""".r 
  }
  
  def escapedChar(implicit state: ParserState): Parser[Inline] = {
    def strToInline(str: String): Inline = {
      str match {
        case " " => Str("\u00A0")
        case "\n" => LineBreak
        case x => Str(x)
      }
    }
    escapedCharPrime ^^ strToInline
  }
}*/