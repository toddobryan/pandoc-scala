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
  
  def listParser[T](implicit man: Manifest[T]): Parser[List[T]] = {
    val itemParser = parsers(man)().asInstanceOf[Parser[T]] 
    ("[" ~> repsep(itemParser, regex("""\s*,\s*"""r)) <~ "]")
  }
  
  def read[T](input: String)(implicit man: Manifest[T]): ParseResult[T] = {    
    parse(parsers(man)().asInstanceOf[Parser[T]], input)
  }
}
