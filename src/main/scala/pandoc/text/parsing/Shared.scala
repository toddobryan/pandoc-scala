package pandoc.text.parsing

import pandoc.text.pretty.charWidth
import pandoc.text.{Block, Para, Plain}

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
  
  def stripTrailingNewlines(str: String): String = {
    str.reverse.dropWhile(_ == '\n').reverse
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
  
  def stripFirstAndLast(s: String): String = {
    s.take(s.length - 1).drop(1)
  }
  
  def camelCaseToHyphenated(s: String): String = {
    def helper(chars: List[Char]): List[Char] = {
      chars match {
        case Nil => Nil
        case a :: b :: rest if (a.isLower && b.isUpper) => a :: '-' :: b.toLower :: helper(rest)
        case a :: rest => a.toLower :: helper(rest)
      }
    }
    helper(s.toList).mkString
  }
  
  def toRomanNumeral(num: Int): String = {
    if (num >= 4000 || num < 0) "?"
    else if (num >= 1000) "M" + toRomanNumeral(num - 1000)
    else if (num >= 900) "CM" + toRomanNumeral(num - 900)
    else if (num >= 500) "D" + toRomanNumeral(num - 500)
    else if (num >= 400) "CD" + toRomanNumeral(num - 400)
    else if (num >= 100) "C" + toRomanNumeral(num - 100)
    else if (num >= 90) "XC" + toRomanNumeral(num - 90)
    else if (num >= 50) "L" + toRomanNumeral(num - 50)
    else if (num >= 40) "XL" + toRomanNumeral(num - 40)
    else if (num >= 10) "X" + toRomanNumeral(num - 10)
    else if (num >= 9) "IX" + toRomanNumeral(num - 9)
    else if (num >= 5) "V" + toRomanNumeral(num - 5)
    else if (num >= 4) "IV" + toRomanNumeral(num - 4)
    else if (num >= 1) "I" + toRomanNumeral(num - 1)
    else ""
  }
  
  def compactify(items: List[List[Block]]): List[List[Block]] = {
    (items.init, items.last) match {
      case (_, Nil) => items
      case (others, last) => last.last match {
        case Para(a) => items.flatten.filter(isPara(_)) match {
          case x :: Nil => others ++ List(last.init ++ List(Plain(a)))
          case _ => items
        }
        case _ => items
      }
    }
  }
  
  def isPara(b: Block): Boolean = {
    b match {
      case Para(_) => true
      case _ => false
    }
  }
}