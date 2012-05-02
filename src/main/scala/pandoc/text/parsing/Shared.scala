package pandoc.text.parsing

import pandoc.text.pretty.charWidth

object Shared {
  def splitBy[A](pred: A => Boolean, lst: List[A]): List[List[A]] = {
    lst match {
      case Nil => Nil
      case _ => {
        val (first, rest) = lst.span(!pred(_))
        val restPrime = rest.dropWhile(pred)
        first :: splitBy(pred, restPrime)
      }
    }
  }
  
  def splitByIndices[A](indices: List[Int], lst: List[A]): List[List[A]] = {
    indices match {
      case Nil => List(lst)
      case x :: xs => {
        val (first, rest) = lst.splitAt(x)
        first :: splitByIndices(xs.map((y: Int) => y - x), rest)
      }
    }
  }
  
  def splitStringByIndices(indices: List[Int], s: String): List[String] = {
    indices match {
      case Nil => List(s)
      case x :: xs => {
        val (first, rest) = splitAtPrime(x, s)
        first :: splitStringByIndices(xs.map((y: Int) => y - x), rest)
      }
    }
  }
  
  def splitAtPrime(n: Int, s: String): (String, String) = {
    s match {
      case "" => ("" , "")
      case xs if (n <= 0) => ("", xs)
      case xs => {
        val (ys, zs) = splitAtPrime(n - charWidth(xs.charAt(0)), xs.substring(1))
        (xs.substring(0, 1) + ys, zs)
      }
    }
  }
  
  def backslashEscapes(cs: List[Char]): Map[Char, String] = {
    Map(cs.map((ch: Char) => ch -> List('\\', ch).mkString): _*)
  }
  
  def escapeStringUsing(escapeTable: Map[Char, String], str: String): String = {
    def helper(chars: List[Char]): String = {
      chars match {
        case Nil => ""
        case c :: cs => {
          val rest = helper(cs)
          escapeTable.get(c) match {
            case Some(esc) => esc + rest
            case None => c.toString + rest
          }
        }
      }  
    }
    helper(str.toList)
  }
  
  def substitute[T](target: List[T], replacement: List[T], list: List[T]): List[T] = {
    (target, replacement, list) match {
      case (_, _ , Nil) => Nil
      case (Nil, _, xs) => xs
      case (_, _, x :: xs) => if (list.startsWith(target)) {
        replacement ++ (substitute(target, replacement, list.drop(target.length)))
      } else {
        x :: substitute(target, replacement, xs)
      }
    }
  }
  
  def removeLeadingTrailingSpace(s: String): String = {
    removeLeadingSpace(removeTrailingSpace(s))
  }

  def removeLeadingSpace(s: String): String = {
    s.dropWhile(" \n\t".contains(_))
  }
  
  def removeTrailingSpace(s: String): String = {
    removeLeadingSpace(s.reverse).reverse
  }
  
}