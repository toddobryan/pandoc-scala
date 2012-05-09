package pandoc.text

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.Reader
import scala.util.parsing.input.Position
import scala.util.parsing.input.CharSequenceReader

class StatefulReader[State, Elem](val state: State, val wrapped: Reader[Elem]) extends Reader[Elem] {
  override def atEnd: Boolean = wrapped.atEnd
  override def first: Elem = wrapped.first
  override def pos: Position = wrapped.pos
  override def rest: StatefulReader[State, Elem] = new StatefulReader[State, Elem](state, wrapped.rest)
  
  override def drop(n: Int): StatefulReader[State, Elem] = new StatefulReader[State, Elem](state, wrapped.drop(n))
  override def offset: Int = wrapped.offset
  override def source: CharSequence = wrapped.source
  
  // State methods
  def updateState(f: State => State): StatefulReader[State, Elem] = {
    new StatefulReader[State, Elem](f(state), wrapped)
  }
  def setState(newState: State): StatefulReader[State, Elem] = {
    new StatefulReader[State, Elem](newState, wrapped)
  }
}

/**
 * Includes all the parsers in Regex, plus some from Parsec for convenience
 */
trait StatefulParsers[State] extends RegexParsers {
  override type Elem = Char
  //override type Input = StatefulReader[State, Elem]
  override def skipWhitespace = false
  
  def StatefulParser[T](f: (StatefulReader[State, Elem]) => ParseResult[T]) = new Parser[T] {
    def apply(in: Input): ParseResult[T] = f(in.asInstanceOf[StatefulReader[State, Elem]])
  }
  
  def choice[A](p1: Parser[A], ps: Parser[A]*): Parser[A] = {
    ps.toList match {
      case Nil => p1
      case p2 :: rest => p1 | choice[A](p2, rest: _*)
    }
  }
  
  def parse[T](parser: T, in: CharSequence): ParseResult[T] = {
    val reader: Reader[Char] = new StatefulReader(ParserState(), new CharSequenceReader(in))
    parse[T](parser, reader)
  }
  
  def getState: Parser[State] = {
    StatefulParser[State]((in: StatefulReader[State, Elem]) => Success(in.state, in))
  }
  def setState(newState: State): Parser[Unit] = {
    StatefulParser[Unit]((in: StatefulReader[State, Elem]) => Success((), in.setState(newState)))
  }
  def getInput: Parser[Reader[Elem]] = {
    StatefulParser[Reader[Elem]]((in: StatefulReader[State, Elem]) => Success(in.wrapped, in))
  }
  def setInput(newInput: Reader[Elem]) = {
    StatefulParser[Unit]((in: StatefulReader[State, Elem]) => Success((), new StatefulReader(in.state, newInput)))
  }
}