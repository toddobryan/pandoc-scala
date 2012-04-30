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

abstract class Result[+T, State, Elem](val state: State, val next: Reader[Elem]) {
  def updateState[NewState](state: NewState): Result[T, NewState, Elem]
  def map[U](f: (T => U)): Result[U, State, Elem]
}
case class Ok[T, State, Elem](result: T, override val state: State, override val next: Reader[Elem])
    extends Result[T, State, Elem](state, next) {
  def updateState[NewState](newState: NewState) = Ok[T, NewState, Elem](result, newState, next)
  def map[U](f: (T => U)) = Ok(f(result), state, next)
}
case class Error[State, Elem](override val state: State, override val next: Reader[Elem], messages: List[String])
    extends Result[Nothing, State, Elem](state, next) {
  def updateState[NewState](newState: NewState) = Error[NewState, Elem](newState, next, messages)
  def map[U](f: (Nothing => U)) = this
}
object Error {
  def apply[T, State, Elem](state: State, in: Reader[Elem], msg: String): Error[State, Elem] = Error(state, in, List(msg))
}

case class <~>[+T1, +T2](t1: T1, t2: T2)

abstract class Parser[+T, +State, Elem] {
  def apply(state: State, in: Reader[Elem]): Result[T, State, Elem]

  def ? : Parser[Option[T], State, Elem] = {
    this ^^ ((t: T) => Some(t)) | new Success(None)
  }
  
  def * : Parser[List[T], State, Elem] = new RepParser(this)
  
  def + : Parser[List[T], State, Elem] = {
    (this <~> this.*) ^^ {
      case t <~> ts => t :: ts
    }
  }
  
  def repsep[U](sep: => Parser[U, State, Elem]): Parser[List[T], State, Elem] = {
    this <~> (sep ~> this).* ^^ {
      case t <~> ts => t :: ts
    }
  }
  
  def <~>[T2](right: => Parser[T2, State, Elem]): Parser[<~>[T, T2], State, Elem] = {
    new SequencedParser(this, right)
  }
  
  def <~[U](right: => Parser[U, State, Elem]): Parser[T, State, Elem] = {
    (this <~> right) ^^ {
      case t <~> u => t
    }
  }
  
  def ~>[U](right: => Parser[U, State, Elem]): Parser[U, State, Elem] = {
    (this <~> right) ^^ {
      case t <~> u => u
    }
  }
  
  def |[U >: T](right: => Parser[U, State, Elem]): Parser[U, State, Elem] = {
    new DisjointParser(this, right)
  }
  
  def ^^[U](f: (T => U)): Parser[U, State, Elem] = new Parser[U, State, Elem] {
    def apply(state: State, in: Reader[Elem]): Result[U, State, Elem] = {
      Parser.this(state, in).map(f)
    }
  }
  
  def ^^^[U](u: => U): Parser[U, State, Elem] = {
    this ^^ ((t: T) => u)
  }
  
  def lookAhead: Parser[Unit, State, Elem] = new Parser[Unit, State, Elem] {
    def apply(state: State, in: Reader[Elem]): Result[Unit, State, Elem] = {
      this(state, in) match {
        case Ok(_, _, _) => Ok((), state, in)
        case e: Error[_, _] => e
      }
    }
  }
  
  def count(exact: Int): Parser[List[T], State, Elem] = count(exact, exact)
  
  def count(min: Int, max: Int) = new Parser[List[T], State, Elem] {
    def apply(state: State, in: Reader[Elem]): Result[List[T], State, Elem] = {
      /* accumulates the answer in reverse order */
      def parseAndAccum(st: State, r: Reader[Elem], acc: List[T]): Result[List[T], State, Elem] = {
        if (acc == max) Ok(acc.reverse, st, r)
        else Parser.this(st, r) match {
          case Ok(t, newState, next) => parseAndAccum(newState, next, t :: acc)
          case e: Error[_, _] => {
            if (acc.length >= min) Ok(acc.reverse, st, r)
            else e
          }
        }
      }
      parseAndAccum(state, in, Nil)
    }
  } 
  
  def X: Parser[Unit, State, Elem] = new Parser[Unit, State, Elem] {
    def apply(state: State, in: Reader[Elem]): Result[Unit, State, Elem] = {
      this(state, in) match {
        case Ok(t, _, _) => Error(state, in, "expected something other than '%s', but found it".format(t))
        case e: Error[_, _] => Ok((), state, in)
      }
    }
  }
  
  def notFollowedBy(p: => Parser[_, State, Elem]): Parser[T, State, Elem] = {
    this <~ p.X
  }
}

class TestParser[Elem, State](pred: (Elem => Boolean), predDesc: String)
		extends Parser[Elem, State, Elem] {
  def apply(state: State, in: Reader[Elem]): Result[Elem, State, Elem] = {
    in.first match {
      case x if pred(x) => Ok(x, state, in.rest)
      case x => Error(state, in, "expected %s, but found '%s'".format(predDesc, x))
    }
  }
}

class LiteralParser[Elem, State](elem: Elem)
  extends TestParser[Elem, State]((e: Elem) => e == elem, "'%s'".format(elem))
  
class ListParser[Elem, State](list: List[Elem]) extends Parser[List[Elem], State, Elem] {
  def apply(state: State, in: Reader[Elem]): Result[List[Elem], State, Elem] = {
    list match {
      case Nil => Ok(Nil, state, in)
      case e :: es => new LiteralParser(e)(state, in) match {
        case Ok(elem, newState, next) => (new ListParser[Elem, State](es) ^^ ((elems: List[Elem]) => elem :: elems))(newState, next)
        case err: Error[_, _] => err
      }
    }
  }
}
  
class Success[T, State, Elem](t: => T) extends Parser[T, State, Elem] {
  def apply(state: State, in: Reader[Elem]): Result[T, State, Elem] = Ok(t, state, in)
}

class Failure[T, State, Elem](msg: String) extends Parser[T, State, Elem] {
  def apply(state: State, in: Reader[Elem]): Result[T, State, Elem] = Error(state, in, msg)
}

class SequencedParser[+T1, +T2, State, Elem](left: => Parser[T1, State, Elem], right: => Parser[T2, State, Elem])
		extends Parser[<~>[T1, T2], State, Elem] {
  def apply(state: State, in: Reader[Elem]): Result[<~>[T1, T2], State, Elem] = {
    left(state, in) match {
      case Ok(t1, newState, next) => right(newState, next) match {
        case Ok(t2, finalState, finalReader) => Ok(new <~>(t1, t2), finalState, finalReader)
        case e: Error[_, _] => e
      }
      case e: Error[_, _] => e
    }
  }
}

class DisjointParser[+T, State, Elem](left: => Parser[T, State, Elem], right: => Parser[T, State, Elem]) 
		extends Parser[T, State, Elem] {
  def apply(state: State, in: Reader[Elem]): Result[T, State, Elem] = {
    left(state, in) match {
      case ok: Ok[_, _, _] => ok
      case _ => right(state, in)
    }
  }  
}

class RepParser[+T, State, Elem](p: => Parser[T, State, Elem]) extends Parser[List[T], State, Elem] {
  def apply(state: State, in: Reader[Elem]): Result[List[T], State, Elem] = {
    p(state, in) match {
      case Ok(t, newState, next) => {
        if (in.currPos == next.currPos) Error(state, in, "the repeat combinator was applied to a parser that consumes no input")
        else this(newState, next) match {
          case Ok(ts, finalState, finalReader) => Ok(t :: ts, finalState, finalReader)
          case e: Error[_, _] => e
        }
      }
      case e: Error[_, _] => Ok(Nil, state, in)
    }
  }
}

/**
 * a set of useful character parsers
 */
object CharParsers {
  def anyChar[State] = new TestParser[Char, State]((c: Char) => true, "a character")
  def lit[State](c: Char) = new TestParser[Char, State](_ == c, "'%s'".format(c))
  def digit[State] = new TestParser[Char, State]((c: Char) => c.isDigit, "a digit")
  def letter[State] = new TestParser[Char, State]((c: Char) => c.isLetter, "a letter")
  def alphaNum[State] = digit[State] | letter[State]
  def noneOf[State](s: String) = new TestParser[Char, State](!s.contains(_), "a character not in '%s'".format(s))
  def oneOf[State](s: String) = new TestParser[Char, State](s.contains(_), "a character in '%s'".format(s))
  def string[State](s: String): Parser[String, State, Char] = new ListParser[Char, State](s.toList) ^^ ((cs: List[Char]) => cs.mkString)
}
