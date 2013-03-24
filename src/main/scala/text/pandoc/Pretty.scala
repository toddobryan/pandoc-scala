package text.pandoc

import Stream.Empty

object Pretty {
  case class RenderState(
      output: Stream[String] = Empty, // in reverse order
      prefix: String = "",
      usePrefix: Boolean = true,
      lineLength: Option[Int] = None,
      column: Int = 0,
      newLines: Int = 2)
  
  sealed abstract class D {
    def isBlank: Boolean = false
  }
  case class Text(len: Int, content: String) extends D {
    override def isBlank: Boolean = content.length > 0 && content.charAt(0).isSpaceChar
  }
  case class Block(width: Int, content: Stream[String]) extends D
  case class Prefixed(prefix: String, doc: Doc) extends D
  case class BeforeNonBlank(doc: Doc) extends D
  case class Flush(doc: Doc) extends D
  case object BreakingSpace extends D {
    override def isBlank: Boolean = true
  }
  case object CarriageReturn extends D {
    override def isBlank: Boolean = true
  }
  case object NewLine extends D {
    override def isBlank: Boolean = true
  }
  case object BlankLine extends D {
    override def isBlank: Boolean = true
  }
  
  case class Doc(content: Stream[D]) {
    def isEmpty: Boolean = content.isEmpty
    def <>(that: Doc): Doc = Doc(this.content ++ that.content)
    def <+>(that: Doc): Doc = {
      if (this.isEmpty) that
      else if (that.isEmpty) this
      else this <> space <> that
    }
    def %%(that: Doc): Doc = { // Haskell's $$
      if (this.isEmpty) that
      else if (that.isEmpty) this
      else this <> cr <> that
    }
    def %+%(that: Doc): Doc = { // Haskell's $+$
      if (this.isEmpty) that
      else if (that.isEmpty) this
      else this <> blankline <> that
    }
    def chomp: Doc = {
      Doc(this.content.reverse.dropWhile(_.isBlank).reverse)
    }
  }
  def empty = Doc(Empty)
  
  def cat(docs: Stream[Doc]): Doc = Doc(docs.flatMap(_.content))
  def hcat = cat _
  def hsep(docs: List[Doc]): Doc = docs.foldRight[Doc](empty)((l: Doc, r: Doc) => l <+> r)
  def vcat(docs: List[Doc]): Doc = docs.foldRight[Doc](empty)((l: Doc, r: Doc) => l %% r)
  def vsep(docs: List[Doc]): Doc = docs.foldRight[Doc](empty)((l: Doc, r: Doc) => l %+% r)
  
  def outp(offset: Int, s: String, state: RenderState): RenderState = {
    if (offset <= 0) {
      val rawPref = state.prefix
      if (state.column == 0 && state.usePrefix && rawPref != "") {
        val pref: Stream[Char] = rawPref.toCharArray.toStream.reverse.dropWhile(_.isSpaceChar).reverse
        state.copy(output = pref.mkString #:: state.output, column = state.column + realLength(pref))
      } else if (offset < 0) {
        state.copy(output = s #:: state.output, column = 0, newLines = state.newLines + 1)
      } else {
        state
      }
    } else {
      val pref = state.prefix
      val statePrime: RenderState = 
        if (state.column == 0 && state.usePrefix && pref != "") {
    	  state.copy(output = pref #:: state.output, column = state.column + realLength(pref.toCharArray.toStream))
        } else {
          state
        }
      statePrime.copy(output = s #:: statePrime.output, column = statePrime.column + offset, newLines = 0)
    }
  }
  
  def render(lineLength: Option[Int], doc: Doc): String = {
    renderDoc(doc, RenderState(lineLength = lineLength)).output.reverse.mkString
  }
  
  def renderDoc(doc: Doc, state: RenderState): RenderState = {
    renderList(doc.content, state)
  }
  
  def renderList(elts: Stream[D], state: RenderState): RenderState = {
    elts match {
      case Empty => state
      case Text(offset, s) #:: xs => renderList(xs, outp(offset, s, state))
      case Prefixed(pref, doc) #:: xs => {
        renderList(xs, renderDoc(doc, state.copy(prefix = state.prefix + pref)).
            copy(prefix = state.prefix))
      }
      case Flush(doc) #:: xs => {
        renderList(xs, renderDoc(doc, state.copy(usePrefix = false)).copy(usePrefix = state.usePrefix))
      }
      case BeforeNonBlank(doc) #:: x0 #:: xs if (x0.isBlank) => renderList(x0 #:: xs, state)
      case BeforeNonBlank(doc) #:: Empty => renderList(Empty, state)
      case BeforeNonBlank(doc) #:: xs => renderList(xs, renderDoc(doc, state))
      case BlankLine #:: xs => {
        val modState = 
          if (state.newLines > 1 || xs == Nil) state
          else if (state.column == 0) outp(-1, "\n", state)
          else outp(-1, "\n", outp(-1, "\n", state))
        renderList(xs, modState)
      }
      case CarriageReturn #:: xs => {
        if (state.newLines > 0 || xs == Nil) renderList(xs, state)
        else renderList(xs, outp(-1, "\n", state))
      }
      case NewLine #:: xs => renderList(xs, outp(-1, "\n", state))
      case BreakingSpace #:: CarriageReturn #:: xs => renderList(CarriageReturn #:: xs, state)
      case BreakingSpace #:: NewLine #:: xs => renderList(NewLine #:: xs, state)
      case BreakingSpace #:: BlankLine #:: xs => renderList(BlankLine #:: xs, state)
      case BreakingSpace #:: BreakingSpace #:: xs => renderList(BreakingSpace #:: xs, state)
      case BreakingSpace #:: xs => {
        val xsPrime = xs.dropWhile(_ == BreakingSpace)
        val next = xsPrime.takeWhile((d: D) => d.isInstanceOf[Text] || d.isInstanceOf[Block])
        val offset = next.map(offsetOf(_)).sum
        state.lineLength match {
          case Some(len) if (len < state.column + 1 + offset) => 
            renderList(xsPrime, outp(-1, "\n", state))
          case _ => renderList(xsPrime, outp(1, " ", state))
        }
      }
      case (b1: Block) #:: (b2: Block) #:: xs => 
        renderList(mergeBlocks(false, b1, b2) #:: xs, state)
      case (b1: Block) #:: BreakingSpace #:: (b2: Block) #:: xs => {
        renderList(mergeBlocks(true, b1, b2) #:: xs, state)
      }
      case Block(width, lns) #:: xs => {
        val indent = state.column - realLength(state.prefix.toCharArray.toStream)
        val modState = 
          if (indent > 0) state.copy(prefix = state.prefix + (" " * indent))
          else state
        renderList(xs, renderDoc(blockToDoc(width, lns), modState).copy(prefix = state.prefix))
      }
    }
  }
  
  def mergeBlocks(addSpace: Boolean, b1: Block, b2: Block): D = {
    (b1, b2) match {
      case (Block(w1, lns1), Block(w2, lns2)) => {
        val width = w1 + w2 + (if (addSpace) 1 else 0)
        val empties = List.fill(math.abs(lns1.length - lns2.length))("")
        def pad(n: Int, s: String): String = s + (" " * (n - realLength(s.toCharArray.toStream)))
        def sp(s: String): String = {
          if (s != "" && addSpace) " " + s
          else s
        }
        val lns = (lns1 ++ empties) zip (lns2 ++ empties).map(sp(_)) map {
          case (l1, l2) => pad(w1, l1 + l2)
        }
        Block(width, lns)
      }
    }
  }
  
  def blockToDoc(width: Int, lns: Stream[String]): Doc = text(lns.mkString("\n"))

  def offsetOf(d: D): Int = {
    d match {
      case Text(offset, _) => offset
      case Block(width, _) => width
      case BreakingSpace => 1
      case _ => 0
    }
  }
  
  def toChunks(s: Stream[Char]): Stream[D] = {
    s match {
      case Empty => Empty
      case x => x.span(_ != '\n') match {
        case (Empty, _ #:: ys) => NewLine +: toChunks(ys)
        case (xs, _ #:: ys) => Text(realLength(xs), xs.mkString) +: NewLine +: toChunks(ys)
        case (xs, Empty) => Stream(Text(realLength(xs), xs.mkString))
      }
    }
  }
  def text(s: String): Doc = {
    Doc(toChunks(s.toCharArray.toStream))
  }
  def char(c: Char): Doc = text(c.toString)
  def space: Doc = Doc(Stream(BreakingSpace))
  def cr: Doc = Doc(Stream(CarriageReturn))
  def blankline: Doc = Doc(Stream(BlankLine))
  def prefixed(prefix: String, doc: Doc): Doc = Doc(Stream(Prefixed(prefix, doc)))
  def flush(doc: Doc): Doc = Doc(Stream(Flush(doc)))
  def nest(indent: Int, doc: Doc): Doc = prefixed(" " * indent, doc)
  def hang(indent: Int, start: Doc, doc: Doc): Doc = start <> nest(indent, doc)
  def beforeNonBlank(doc: Doc): Doc = Doc(Stream(BeforeNonBlank(doc)))
  def noWrap(doc: Doc): Doc = {
    val unwrapped = doc.content map {(d: D) =>
      d match {
        case BreakingSpace => Text(1, " ")
        case x => x
      }
    }
    Doc(unwrapped)
  }
  def offset(doc: Doc): Int = render(None, doc).split('\n').toList.
	  map((s: String) => realLength(s.toCharArray.toStream)).foldLeft(0)((l: Int, r: Int) => math.max(l, r))
  def block(filler: (String => String), width: Int)(doc: Doc): Doc = {
    Doc(Stream(Block(width, chop(width, render(Some(width), doc)).map(filler(_)))))
  }
  def lblock(width: Int, doc: Doc): Doc = block((s: String) => s, width)(doc)
  def rblock(width: Int, doc: Doc): Doc = {
    block((s: String) => (" " * (width - realLength(s.toCharArray.toStream))) + s, width)(doc)
  }
  def cblock(width: Int, doc: Doc): Doc = {
    block((s: String) => (" " * ((width - realLength(s.toCharArray.toStream)) / 2)) + s, width)(doc)
  }
  def height(doc: Doc): Int = render(None, doc).split('\n').length
  def chop(width: Int, s: String): Stream[String] = {
    if (s == "") Empty
    else s.toCharArray.toStream.span(_ != '\n') match {
      case (xs, ys) if (realLength(xs) <= width) => ys match {
        case Empty => Stream(xs.mkString)
        case _ #:: Empty => Stream(xs.mkString, "")
        case _ #:: zs => xs.mkString #:: chop(width, zs.mkString)
      }
      case (xs, ys) => xs.take(width).mkString #:: chop(width, (xs ++ ys).drop(width).mkString)
    }
  }
    
  def inside(start: Doc, end: Doc)(contents: Doc): Doc = start <> contents <> end
  def braces(doc: Doc): Doc = inside(char('{'), char('}'))(doc)
  def brackets(doc: Doc): Doc = inside(char('['), char(']'))(doc)
  def parens(doc: Doc): Doc = inside(char('('), char(')'))(doc)
  def quotes(doc: Doc): Doc = inside(char('\''), char('\''))(doc)
  def doubleQuotes(doc: Doc): Doc = inside(char('"'), char('"'))(doc)
  
  def charWidth(c: Char): Int = {
    if (c < 0x0300) 1
    else if (c >= 0x0300 && c <= 0x036f) 0
    else if (c >= 0x0370 && c <= 0x10FC) 1
    else if (c >= 0x1100 && c <= 0x115F) 2
    else if (c >= 0x1160 && c <= 0x11A2) 1
    else if (c >= 0x11A3 && c <= 0x11A7) 2
    else if (c >= 0x11A8 && c <= 0x11F9) 1
    else if (c >= 0x11FA && c <= 0x11FF) 2
    else if (c >= 0x1200 && c <= 0x2328) 1
    else if (c >= 0x2329 && c <= 0x232A) 2
    else if (c >= 0x232B && c <= 0x2E31) 1
    else if (c >= 0x2E80 && c <= 0x303E) 2
    else if (c == 0x303F) 1
    else if (c >= 0x3041 && c <= 0x3247) 2
    else if (c >= 0x3248 && c <= 0x324F) 1 // ambiguous
    else if (c >= 0x3250 && c <= 0x4DBF) 2
    else if (c >= 0x4DC0 && c <= 0x4DFF) 1
    else if (c >= 0x4E00 && c <= 0xA4C6) 2
    else if (c >= 0xA4D0 && c <= 0xA95F) 1
    else if (c >= 0xA960 && c <= 0xA97C) 2
    else if (c >= 0xA980 && c <= 0xABF9) 1
    else if (c >= 0xAC00 && c <= 0xD7FB) 2
    else if (c >= 0xD800 && c <= 0xDFFF) 1
    else if (c >= 0xE000 && c <= 0xF8FF) 1 // ambiguous
    else if (c >= 0xF900 && c <= 0xFAFF) 2
    else if (c >= 0xFB00 && c <= 0xFDFD) 1
    else if (c >= 0xFE00 && c <= 0xFE0F) 1 // ambiguous
    else if (c >= 0xFE10 && c <= 0xFE19) 2
    else if (c >= 0xFE20 && c <= 0xFE26) 1
    else if (c >= 0xFE30 && c <= 0xFE6B) 2
    else if (c >= 0xFE70 && c <= 0x16A38) 1
    else if (c >= 0x1B000 && c <= 0x1B001) 2
    else if (c >= 0x1D000 && c <= 0x1F1FF) 1
    else if (c >= 0x1F200 && c <= 0x1F251) 2
    else if (c >= 0x1F300 && c <= 0x1F773) 1
    else if (c >= 0x20000 && c <= 0x3FFFD) 2
    else 1
  }
  
  def realLength(s: Stream[Char]): Int = {
    s.map(charWidth _).foldLeft[Int](0)((l: Int, r: Int) => l + r)
  }
}
