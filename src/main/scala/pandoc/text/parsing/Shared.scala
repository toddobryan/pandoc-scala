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
  
  def removeLeadingSpace(s: String): String = {
    s.dropWhile(" \n\t".contains(_))
  }
  
  def removeTrailingSpace(s: String): String = {
    removeLeadingSpace(s.reverse).reverse
  }

}