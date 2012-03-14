package pandoc.util

import scala.collection.mutable
import scala.util.parsing.combinator.RegexParsers

import pandoc.text

object Reader extends RegexParsers {
  val parsers = mutable.Map[Manifest[_], () => Parser[_]](
    manifest[Int] -> (() => regex("""-?[1-9][0-9]*"""r).map(_.toInt)),
    manifest[Double] -> (() => regex("""-?([0-9]+\.?|[0-9]*\.[0-9]+)"""r).map(_.toDouble)),
    manifest[Boolean] -> (() => regex("""true|false"""r).map(_.toBoolean)),
    manifest[String] -> (() => regex("""\"([^"\\]|\[bfnrt\\'"])*\""""r))
  )
  text.initializeParsers()
  
  def getParser[T](implicit man: Manifest[T]): Parser[T] = {
    parsers(man)().asInstanceOf[Parser[T]]
  }
  
  def openParser(s: String): Parser[_] = {
    (literal(s) ~ regex("""\s*"""r))
  }
  
  val comma: Parser[_] = regex("""\s*,\s*"""r)
  
  def closeParser(s: String): Parser[_] = {
    (regex("""\s*"""r) ~ literal(s))
  }
  
  def listParser[T](implicit man: Manifest[T]): Parser[List[T]] = {
    val itemParser = parsers(man)().asInstanceOf[Parser[T]] 
    (openParser("[") ~> repsep(itemParser, comma) <~ closeParser(")"))
  }
  
  def dupleParser[A, B](aParser: Parser[A], bParser: Parser[B])(implicit manA: Manifest[A], manB: Manifest[B]): Parser[(A, B)] = {
    ((openParser("(") ~> aParser <~ comma) ~ (bParser <~ closeParser(")"))).map({
      case a ~ b => (a, b)
    })
  }
  
  def tripleParser[A, B, C](aParser: Parser[A], bParser: Parser[B], cParser: Parser[C])
  		(implicit manA: Manifest[A], manB: Manifest[B], manC: Manifest[C]): Parser[(A,B,C)] = {
    ((openParser("(") ~> aParser <~ comma) ~ (bParser <~ comma) ~ (cParser <~ closeParser(")"))).map {
      case a ~ b ~ c => (a, b, c)
    }
  }
  
  def labelWithContent[T, U](label: String, resultType: Function1[List[T], U])(implicit man: Manifest[T]): (() => Parser[U]) = {
    () => (regex("""%s\s+""".format(label).r) ~> listParser[T]).map(resultType.apply(_))
  }
    
  def read[T](input: String)(implicit man: Manifest[T]): ParseResult[T] = {    
    parse(parsers(man)().asInstanceOf[Parser[T]], input)
  }
  
  def parsedPlusRest[T](res: ParseResult[T]): (T, String) = {
    (res.get, res.next.source.subSequence(res.next.offset, res.next.source.length).toString)
  }
}
