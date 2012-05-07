package pandoc.text

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.Reader
import scala.util.parsing.input.Position

class StatefulReader[State, Elem](val state: State, val wrapped: Reader[Elem]) extends Reader[Elem] {
  def atEnd: Boolean = wrapped.atEnd
  def first: Elem = wrapped.first
  def pos: Position = wrapped.pos
  def rest: StatefulReader[State, Elem] = new StatefulReader[State, Elem](state, wrapped.rest)
  
  def drop(n: Int): StatefulReader[State, Elem] = new StatefulReader[State, Elem](state, wrapped.drop(n))
  def offset: Int = wrapped.offset
  def source: CharSequence = wrapped.source
  
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
  type Elem = Char
  type Input = StatefulReader[State, Elem]
  override def skipWhitespace = false
  
  override def Parser[T](f: (Input) => ParseResult[T]) = new Parser[T] {
    def apply(in: Input): ParseResult[T] = f(in)
  }
  
  def choice[A](p1: Parser[A], ps: Parser[A]*): Parser[A] = {
    ps.toList match {
      case Nil => p1
      case p2 :: rest => p1 | choice[A](p2, rest: _*)
    }
  }
  
  def getState: Parser[State] = {
    Parser[State]((in: Input) => Success(in.state, in))
  }
  def setState(newState: State): Parser[Unit] = {
    Parser[Unit]((in: Input) => Success((), in.setState(newState)))
  }
  def getInput: Parser[Reader[Elem]] = {
    Parser[Reader[Elem]]((in: Input) => Success(in.wrapped, in))
  }
  def setInput(newInput: Reader[Elem]) = {
    Parser[Unit]((in: Input) => Success((), new StatefulReader(in.state, newInput)))
  }
}