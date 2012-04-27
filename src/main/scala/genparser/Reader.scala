package genparser

import scala.collection.immutable.Stream

trait Reader[Elem] {  
  def atEnd: Boolean
  def first: Elem
  def rest: Reader[Elem]
  def drop(n: Int): Reader[Elem] = {
    def dropAcc(m: Int, next: Reader[Elem]): Reader[Elem] = {
      if (m == 0) next
      else dropAcc(m - 1, next.rest) 
    }
    if (n <= 0) this
    else dropAcc(n - 1, rest)
  }
  def currPos: Pos
}

case class Pos(line: Int, column: Int) {
  def update(c: Char): Pos = c match {
    case '\n' => Pos(line + 1, 1)
    case _ => Pos(line, column + 1)
  }
}

class CharReader(val source: Stream[Char], pos: Pos) extends Reader[Char] {
  def atEnd: Boolean = source.isEmpty
  def first: Char = source.head
  def rest: CharReader = new CharReader(source.tail, currPos.update(first))
  def currPos: Pos = pos
  
  def canEqual(other: Any) = other.isInstanceOf[CharReader]
    
  override def equals(other: Any) = {
    other match {
      case that: CharReader => (that.canEqual(this)
                                && currPos == that.currPos 
                                && source == that.source)
      case _ => false
    }
  }
  
  override def toString: String = {
    "CharReader(%s, %s)".format(source.toString, currPos.toString)
  }
}
object CharReader {
  def apply(source: Stream[Char], pos: Pos) = new CharReader(source, pos)
  def apply(input: String, pos: Pos) = new CharReader(Stream[Char]() ++ input, pos)
}

abstract class Result[T, State, Elem](val state: State, val next: Reader[Elem]) {
  def updateState[NewState](state: NewState): Result[T, NewState, Elem]
}
case class Ok[T, State, Elem](result: T, override val state: State, override val next: Reader[Elem])
    extends Result[T, State, Elem](state, next) {
  def updateState[NewState](newState: NewState) = Ok[T, NewState, Elem](result, newState, next)
}
case class Error[T, State, Elem](override val state: State, override val next: Reader[Elem], messages: List[String])
    extends Result[T, State, Elem](state, next) {
  def updateState[NewState](newState: NewState) = Error[T, NewState, Elem](newState, next, messages)
}

abstract class Parser[T, State, Elem] {
  def apply(state: State, in: Reader[Elem]): Result[T, State, Elem]

  def probe: Parser[T, State, Elem] = new Parser[T, State, Elem] {
    def apply(state: State, in: Reader[Elem]): Result[T, State, Elem] = {
      Parser.this.apply(state, in) match {
        case Ok(res, st, nxt)  => Ok(res, st, nxt)
        case Error(st, next, msgs) => Error(state, in, msgs)
      }
    }
  }
}

object Parser {
  def literal[Elem, State](elem: Elem) = new Parser[Elem, State, Elem] {
    def apply(state: State, in: Reader[Elem]): Result[Elem, State, Elem] = {
      in.first match {
        case x if (x == elem) => Ok(elem, state, in.rest)
        case _ => Error(state, in.rest, List("expected '%s', but found '%s'".format(elem, in.first)))
      }
    }
  }
}

