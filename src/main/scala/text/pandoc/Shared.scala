package text.pandoc

import java.io.File
import java.text.SimpleDateFormat
import java.util.Date
import java.text.ParseException
import scalaz.State
import scalaz.Scalaz._
import text.highlighting.kate.Types.{Style, Pygments}
import Generic.{bottomUp, topDown, queryWith}
import Pretty.charWidth

import definition._

import Stream.Empty

object Shared {
  def splitBy[A](pred: (A) => Boolean, lst: Stream[A]): Stream[Stream[A]] = {
    lst match {
      case Empty => Empty
      case _ => {
        val (first, rest) = lst.span(!pred(_))
        val restPrime = rest.dropWhile(pred)
        first #:: splitBy(pred, restPrime)
      }
    }
  }
  
  def splitByIndices[A](indices: Stream[Int], lst: Stream[A]): Stream[Stream[A]] = {
    indices match {
      case Empty => Stream(lst)
      case x #:: xs => {
        val (first, rest) = lst.splitAt(x)
        first #:: splitByIndices(xs.map((y: Int) => y - x), rest)
      }
    }
  }
  
  def splitStringByIndices(indices: Stream[Int], s: String): Stream[String] = {
    indices match {
      case Empty => Stream(s)
      case x #:: xs => {
        val (first, rest) = splitAtPrime(x, s)
        first #:: splitStringByIndices(xs.map((y: Int) => y - x), rest)
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
  
  def substitute[T](target: Stream[T], replacement: Stream[T], list: Stream[T]): Stream[T] = {
    (target, replacement, list) match {
      case (_, _ , Empty) => Empty
      case (Empty, _, xs) => xs
      case (_, _, x #:: xs) => if (list.startsWith(target)) {
        replacement ++ (substitute(target, replacement, list.drop(target.length)))
      } else {
        x #:: substitute(target, replacement, xs)
      }
    }
  }
  
  def backslashEscapes(cs: Stream[Char]): Map[Char, String] = {
    Map(cs.map((ch: Char) => ch -> Stream('\\', ch).mkString): _*)
  }
  
  def escapeStringUsing(escapeTable: Map[Char, String], str: String): String = {
    def helper(chars: Stream[Char]): String = {
      chars match {
        case Empty => ""
        case c #:: cs => {
          val rest = helper(cs)
          escapeTable.get(c) match {
            case Some(esc) => esc + rest
            case None => c.toString + rest
          }
        }
      }  
    }
    helper(str.toStream)
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
    def helper(chars: Stream[Char]): Stream[Char] = {
      chars match {
        case Empty => Empty
        case a #:: b #:: rest if (a.isLower && b.isUpper) => a #:: '-' #:: b.toLower #:: helper(rest)
        case a #:: rest => a.toLower #:: helper(rest)
      }
    }
    helper(s.toStream).mkString
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
  
  def escapeUriChar(pred: (Char => Boolean), c: Char): String = {
    val hex = c.toInt.toHexString
    val twoCharHex = if (hex.length < 2) "0" + hex else hex
    if (pred(c)) c.toString
    else "%" + twoCharHex
  }
  
  def escapeUriString(pred: (Char => Boolean), s: String): String = {
    s.toList.map(escapeUriChar(pred, _)).mkString
  }
  
  def isSpace(c: Char): Boolean = {
    c.isSpaceChar || "\t\n\r\f\u000B".contains(c)
  }
  
  def escapeUri(s: String): String = escapeUriString(!isSpace(_), s)
  
  def tabFilter(tabStop: Int, str: String): String = {
    def go(spsToNextStop: Int, cs: Stream[Char]): Stream[Char] = {
      (spsToNextStop, cs) match {
        case (_, Empty) => Empty
        case (_, '\n' #:: xs) => '\n' #:: go(tabStop, xs)
        case (_, '\r' #:: '\n' #:: xs) => '\n' #:: go(tabStop, xs)
        case (_, '\r' #:: xs) => '\n' #:: go(tabStop, xs)
        case (_, '\t' #:: xs) => if (tabStop == 0) '\t' #:: go(tabStop, xs)
                                else Stream.fill(spsToNextStop)(' ') ++ go(tabStop, xs)
        case (1, x #:: xs) => x #:: go(tabStop, xs)
        case (_, x #:: xs) => x #:: go(spsToNextStop - 1, xs)
      }
    }
    go(tabStop, str.toStream).mkString
  }
  
  private def parseDateTime(s: String, format: String): Option[Date] = {
    val parser = new SimpleDateFormat(format)
    try {
      Some(parser.parse(s))
    } catch {
      case e: ParseException => None
    }   
  }
  
  def normalizeDate(s: String): Option[String] = {
    val default = new SimpleDateFormat("yyyy-MM-dd")
    val formats: List[String] = List( 
        new SimpleDateFormat().toPattern, // "%x"
        "M/d/y", // %m/%d/%Y" and "%D"
        "y-M-d", // "%F", 
        "d MMM y", // "%d %b %Y"
        "d MMMM y", // "%d %B %Y"
        "MMM. d, yyyy", // %b. %d, %Y"
        "MMMM d, yyyy") // "%B %d, %Y"
    formats.find(parseDateTime(s, _).isDefined).map(default.format(_))
  }
  
  def orderedListMarkers(attrs: ListAttributes): Stream[String] = {
    val alphabet = "abcdefghijklmnopqrstuvwxyz"
    Stream.from(attrs.num).map((num: Int) => attrs.style match {
      case DefaultStyle => num.toString
      case Example => num.toString
      case Decimal => num.toString
      case UpperAlpha => alphabet.toUpperCase.substring((num - 1) % 26, (num - 1) % 26 + 1)
      case LowerAlpha => alphabet.substring((num - 1) % 26, (num - 1) % 26 + 1)
      case UpperRoman => toRomanNumeral(num)
      case LowerRoman => toRomanNumeral(num).toLowerCase
    }).map((str: String) => attrs.delim match {
      case DefaultDelim => str + "."
      case Period => str + "."
      case OneParen => str + ")"
      case TwoParens => "(" + str + ")"
    })
  }
  
  def normalizeSpaces(in: Stream[Inline]): Stream[Inline] = {
    def cleanup(inlines: Stream[Inline]): Stream[Inline] = inlines match {
      case Empty => Empty
      case Space #:: rest => {
        val restPrime = rest.dropWhile(isSpaceOrEmpty(_))
        restPrime match {
          case Empty => Empty
          case _ => Space #:: cleanup(rest)
        }
      }
      case Str("") #:: rest => cleanup(rest)
      case x #:: rest => x #:: cleanup(rest)
    }
    cleanup(in.dropWhile(isSpaceOrEmpty(_)))
  }
  
  def isSpaceOrEmpty(in: Inline): Boolean = in match {
    case Space => true
    case Str("") => true
    case _ => false
  }
  
  def normalize(doc: Pandoc): Pandoc = {
    def remTrSpThenRemEmpIn(x: Any): Any = x match {
      case l: Stream[_] if (l.forall(_.isInstanceOf[Inline])) => 
          removeEmptyInlines(removeTrailingInlineSpaces(x.asInstanceOf[Stream[Inline]]))
      case _ => x
    }
    def consInlines(x: Any): Any = x match {
      case l: Stream[_] if (l.forall(_.isInstanceOf[Inline])) =>
          consolidateInlines(x.asInstanceOf[Stream[Inline]])
      case _ => x
    }
    def remEmpBls(x: Any): Any = x match {
      case l: Stream[_] if (l.forall(_.isInstanceOf[Block])) =>
          removeEmptyBlocks(x.asInstanceOf[Stream[Block]])
      case _ => x
    }
    topDown(remEmpBls, topDown(consInlines, bottomUp(remTrSpThenRemEmpIn, doc)))
  }
  
  def removeEmptyBlocks(in: Stream[Block]): Stream[Block] = in match {
    case EmptyBlock #:: xs => removeEmptyBlocks(xs)
    case BulletList(Empty) #:: xs => removeEmptyBlocks(xs)
    case OrderedList(_, Empty) #:: xs => removeEmptyBlocks(xs)
    case DefinitionList(Empty) #:: xs => removeEmptyBlocks(xs)
    case RawBlock(_, "") #:: xs => removeEmptyBlocks(xs)
    case x #:: xs => x #:: removeEmptyBlocks(xs)
    case Empty => Empty
  }
  
  def removeEmptyInlines(in: Stream[Inline]): Stream[Inline] = in match {
    case Emph(Empty) #:: zs => removeEmptyInlines(zs)
    case Strong(Empty) #:: zs => removeEmptyInlines(zs)
    case Subscript(Empty) #:: zs => removeEmptyInlines(zs)
    case Superscript(Empty) #:: zs => removeEmptyInlines(zs)
    case SmallCaps(Empty) #:: zs => removeEmptyInlines(zs)
    case Strikeout(Empty) #:: zs => removeEmptyInlines(zs)
    case RawInline(_, "") #:: zs => removeEmptyInlines(zs)
    case Code(_, "") #:: zs => removeEmptyInlines(zs)
    case Str("") #:: zs => removeEmptyInlines(zs)
    case x #:: xs => x #:: removeEmptyInlines(xs)
    case Empty => Empty
  }
  
  def removeTrailingInlineSpaces(in: Stream[Inline]): Stream[Inline] = {
    removeLeadingInlineSpaces(in.reverse).reverse
  }
  
  def removeLeadingInlineSpaces(in: Stream[Inline]): Stream[Inline] = {
    in.dropWhile(isSpaceOrEmpty(_))
  }
  
  private def stripStrs(in: Stream[Inline]): (Stream[Str], Stream[Inline]) = in match {
    case Empty => (Empty, Empty)
    case x #:: xs => x match {
      case str: Str => {
        val (restStrs, rest) = stripStrs(xs)
        (str #:: restStrs, rest)
      }
      case _ => (Empty, x #:: xs)
    }
  }
  
  def consolidateInlines(in: Stream[Inline]): Stream[Inline] = in match {
    case Str(x) #:: ys => {
      val (strs, rest) = stripStrs(ys)
      (x #:: (strs.map(_.str))).mkString match {
        case "" => consolidateInlines(rest)
        case s: String => Str(s) #:: consolidateInlines(rest)
      }
    }
    case Space #:: ys => Space #:: consolidateInlines(ys.dropWhile(_ == Space))
    case Emph(xs) #:: Emph(ys) #:: zs => consolidateInlines(Emph(xs ++ ys) #:: zs)
    case Strong(xs) #:: Strong(ys) #:: zs => consolidateInlines(Strong(xs ++ ys) #:: zs)
    case Subscript(xs) #:: Subscript(ys) #:: zs => consolidateInlines(Subscript(xs ++ ys) #:: zs)
    case Superscript(xs) #:: Superscript(ys) #:: zs => consolidateInlines(Superscript(xs ++ ys) #:: zs)
    case SmallCaps(xs) #:: SmallCaps(ys) #:: zs => consolidateInlines(SmallCaps(xs ++ ys) #:: zs)
    case Strikeout(xs) #:: Strikeout(ys) #:: zs => consolidateInlines(Strikeout(xs ++ys) #:: zs)
    case RawInline(f1, s1) #:: RawInline(f2, s2) #:: zs if (f1 == f2) => consolidateInlines(RawInline(f1, s1 + s2) #:: zs)
    case Code(f1, s1) #:: Code(f2, s2) #:: zs if (f1 == f2) => consolidateInlines(Code(f1, s1 + s2) #:: zs)
    case x #:: xs => x #:: consolidateInlines(xs)
    case Empty => Empty
  }
  
  def stringify(inlines: Stream[Inline]): String = {
    def go: PartialFunction[Any, Any] = {
      case Space => " "
      case Str(x) => x
      case Code(_, x) => x
      case Math(_, x) => x
      case LineBreak => " "
      case x: Inline => ""
    }
    queryWith(go, inlines).mkString
  }
  
  def compactify(items: Stream[Stream[Block]]): Stream[Stream[Block]] = {
    (items.init, items.last) match {
      case (_, Empty) => items
      case (others, last) => last.last match {
        case Para(a) => items.flatten.filter(isPara(_)) match {
          case x #:: Empty => others ++ Stream(last.init ++ Stream(Plain(a)))
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
  
  abstract class Element
  case class Blk(block: Block) extends Element
  case class Sec(lvl: Int, num: Stream[Int], ident: String, label: Stream[Inline], contents: Stream[Element]) extends Element
  
  def inlineListToIdentifier(in: Stream[Inline]): String = {
    def nbspToSp(ch: Char) = if (ch == '\u00A0') ' ' else ch
    stringify(in).filter((c: Char) => c.isLetterOrDigit || "_-. ".contains(c)).
      map((c: Char) => nbspToSp(c.toLower))
  }
  
  def hierarchicalize(blocks: Stream[Block]): Stream[Element] = {
    hierarchicalizeWithIds(blocks) ! (Empty, Empty)
  }
  
  def hierarchicalizeWithIds(blocks: Stream[Block]): State[(Stream[Int], Stream[String]), Stream[Element]] = {
    blocks match {
      case Empty => state((st: (Stream[Int], Stream[String])) => (st, Empty))
      case Header(level, titlePrime) #:: xs => for {
        lastNumAndUsedIdents <- init[(Stream[Int], Stream[String])]
        (lastNum, usedIdents) = lastNumAndUsedIdents
        ident = uniqueIdent(titlePrime, usedIdents)
        lastNumPrime = lastNum.take(level)
        newNum = if (lastNumPrime.length >= level) lastNumPrime.init :+ (lastNumPrime.last + 1)
                 else lastNum ++ List.fill(level - lastNum.length - 1)(0) :+ 1
        _ <- put[(Stream[Int], Stream[String])]((newNum, (ident #:: usedIdents)))
        (sectionContents, rest) = xs.span((blk: Block) => !headerLtEq(level, blk))
        sectionContentsPrime <- hierarchicalizeWithIds(sectionContents)
        restPrime <- hierarchicalizeWithIds(rest)
      } yield Sec(level, newNum, ident, titlePrime, sectionContentsPrime) #:: restPrime
      case x #:: rest => for {
        restPrime <- hierarchicalizeWithIds(rest)
      } yield Blk(x) #:: restPrime
    }
  }
  
  def headerLtEq(level: Int, block: Block): Boolean = {
    block match {
      case Header(l, _) => (l <= level)
      case _ => false
    }
  }
  
  def uniqueIdent(titlePrime: Stream[Inline], usedIdents: Stream[String]): String = {
    val baseIdent = inlineListToIdentifier(titlePrime) match {
      case "" => "section"
      case x => x
    }
    def numIdent(n: Int): String = baseIdent + "-" + n.toString
    if (usedIdents contains baseIdent) {
      (1 to 60000).find((x: Int) => !usedIdents.contains(numIdent(x))) match {
        case Some(x) => numIdent(x)
        case None => baseIdent
      }
    } else baseIdent
  }
  
  def isHeaderBlock(block: Block): Boolean = block.isInstanceOf[Header]
  
  def headerShift[T](n: Int, doc: Pandoc): Pandoc = {
    def shift: (Any) => Any = {
      case Header(l, c) => Header(l + n, c)
      case x => x
    }
    bottomUp(shift, doc)
  }
  
  sealed abstract class HtmlMathMethod
  case object PlainMath extends HtmlMathMethod
  case class LaTeXMathML(url: Option[String]) extends HtmlMathMethod
  case class JsMath(url: Option[String]) extends HtmlMathMethod
  case object GladTeX extends HtmlMathMethod
  case class WebTeX(url: String) extends HtmlMathMethod
  case class MathML(url: Option[String]) extends HtmlMathMethod
  case class MathJax(url: String) extends HtmlMathMethod

  sealed abstract class CiteMethod
  case object Citeproc extends CiteMethod
  case object Natbib extends CiteMethod
  case object Biblatex extends CiteMethod

  sealed abstract class ObfuscationMethod
  case object NoObfuscation extends ObfuscationMethod
  case object ReferenceObfuscation extends ObfuscationMethod
  case object JavaScriptObfuscation extends ObfuscationMethod

  sealed abstract class HtmlSlideVariant
  case object S5Slides extends HtmlSlideVariant
  case object SlidySlides extends HtmlSlideVariant
  case object DZSlides extends HtmlSlideVariant
  case object NoSlides extends HtmlSlideVariant

  // pulled Boolean options into separate class, because there are too many
  // (more than 22)
  case class WriterSwitchOptions(
      standalone: Boolean = false,
      tableOfContents: Boolean = false,
      incremental: Boolean = false,
      xeTeX: Boolean = false,
      ignoreNotes: Boolean = false,
      numberSections: Boolean = false,
      sectionDivs: Boolean = false,
      strictMarkdown: Boolean = false,
      referenceLinks: Boolean = false,
      wrapText: Boolean = true,
      literateHaskell: Boolean = false,
      html5: Boolean = false,
      beamer: Boolean = false,
      chapters: Boolean = false,
      listings: Boolean = false,
      highlight: Boolean = false,
      setextHeaders: Boolean = true)
      
  case class WriterOptions(
      switches: WriterSwitchOptions = WriterSwitchOptions(),
      template: String = "",
      variables: List[(String, String)] = Nil,
      epubMetadata: String = "",
      tabStop: Int = 4,
      slideVariant: HtmlSlideVariant = NoSlides,
      htmlMathMethod: HtmlMathMethod = PlainMath,
      columns: Int = 72,
      emailObfuscation: ObfuscationMethod = JavaScriptObfuscation,
      identifierPrefix: String = "",
      sourceDirectory: File = new File("."),
      userDataDir: Option[File] = None,
      citeMethod: CiteMethod = Citeproc,
      biblioFiles: List[File] = Nil,
      slideLevel: Option[Int] = None,
      highlightStyle: Style = Pygments)
}

// not translated:
// inDirectory
// findDataFile
// readDataFile
// err
// warn