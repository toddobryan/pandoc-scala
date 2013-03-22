package text.pandoc

import definition._

import Stream.Empty

object Builder {
  def doc(blocks: Stream[Block]): Pandoc = Pandoc(Meta(Empty, Empty, Empty), blocks)
  
  def setTitle(t: Stream[Inline], doc: Pandoc): Pandoc = {
    doc.copy(meta=doc.meta.copy(title=t))
  }
  
  def setAuthors(as: Stream[Stream[Inline]], doc: Pandoc): Pandoc = {
    doc.copy(meta=doc.meta.copy(authors=as))
  }
  
  def setDate(d: Stream[Inline], doc: Pandoc): Pandoc = {
    doc.copy(meta=doc.meta.copy(date=d))
  }
  
  def text(str: String): Stream[Inline] = {
    def isSpace(c: Char): Boolean = " \n\t".contains(c)
    def helper(cs: Stream[Char]): Stream[Inline] = cs match {
      case Empty => Empty
      case x #:: xs if isSpace(x) => Space #:: helper(xs.dropWhile(isSpace _))
      case x => {
        val (s, rest) = x.span(!isSpace(_))
        Str(s.mkString) #:: helper(rest)
      }
    }
    helper(str.toStream)
  }
  
  def str(s: String): Stream[Inline] = Stream(Str(s))
  
  def emph(ins: Stream[Inline]): Stream[Inline] = Stream(Emph(ins))
  
  def strong(ins: Stream[Inline]): Stream[Inline] = Stream(Strong(ins))
  
  def strikeout(ins: Stream[Inline]): Stream[Inline] = Stream(Strikeout(ins))
  
  def superscript(ins: Stream[Inline]): Stream[Inline] = Stream(Superscript(ins))
  
  def subscript(ins: Stream[Inline]): Stream[Inline] = Stream(Subscript(ins))
  
  def smallCaps(ins: Stream[Inline]): Stream[Inline] = Stream(SmallCaps(ins))
  
  def singleQuoted(ins: Stream[Inline]): Stream[Inline] = quoted(SingleQuote, ins)
  
  def doubleQuoted(ins: Stream[Inline]): Stream[Inline] = quoted(DoubleQuote, ins)
  
  def quoted(typ: QuoteType, ins: Stream[Inline]): Stream[Inline] = Stream(Quoted(typ, ins))
  
  def cite(cts: Stream[Citation], ins: Stream[Inline]): Stream[Inline] = Stream(Cite(cts, ins))
  
  def codeWith(attrs: Attr, str: String): Stream[Inline] = Stream(Code(attrs, str))
  
  def code(str: String): Stream[Inline] = codeWith(NullAttr, str)
  
  def space: Stream[Inline] = Stream(Space)
  
  def lineBreak: Stream[Inline] = Stream(LineBreak)
  
  def math(str: String): Stream[Inline] = Stream(Math(InlineMath, str))
  
  def displayMath(str: String): Stream[Inline] = Stream(Math(DisplayMath, str))
  
  def rawInline(format: Format, str: String): Stream[Inline] = Stream(RawInline(format, str))
  
  def link(url: String, title: String, label: Stream[Inline]): Stream[Inline] = {
    Stream(Link(label, Target(url, title)))
  }
  
  def image(url: String, title: String, alt: Stream[Inline]): Stream[Inline] = {
    Stream(Image(alt, Target(url, title)))
  }
  
  def note(blks: Stream[Block]): Stream[Inline] = Stream(Note(blks))
  
  def para(ins: Stream[Inline]): Stream[Block] = Stream(Para(ins))
  
  def plain(ins: Stream[Inline]): Stream[Block] = Stream(Plain(ins))
  
  def codeBlockWith(attrs: Attr, str: String): Stream[Block] = {
    Stream(CodeBlock(attrs, str))
  }
  
  def codeBlock(str: String): Stream[Block] = codeBlockWith(NullAttr, str)
  
  def rawBlock(format: Format, str: String): Stream[Block] = Stream(RawBlock(format, str))
  
  def blockQuote(blks: Stream[Block]): Stream[Block] = {
    Stream(BlockQuote(blks))
  }
  
  def orderedListWith(attrs: ListAttributes, items: Stream[Stream[Block]]): Stream[Block] = {
    Stream(OrderedList(attrs, items))
  }
  
  def orderedList(items: Stream[Stream[Block]]): Stream[Block] = {
    orderedListWith(ListAttributes(1, DefaultStyle, DefaultDelim), items)
  }
  
  def bulletList(items: Stream[Stream[Block]]): Stream[Block] = Stream(BulletList(items))
  
  def definitionList(items: Stream[DefnItem]): Stream[Block] = {
    Stream(DefinitionList(items))
  }
  
  def header(level: Int, ins: Stream[Inline]): Stream[Block] = {
    Stream(Header(level, ins))
  }
  
  def horizontalRule: Stream[Block] = Stream(HorizontalRule)
  
  def table(caption: Stream[Inline], 
            cellSpecs: Stream[(Alignment, Double)],
            headers: Stream[TableCell],
            rows: Stream[Stream[TableCell]]): Stream[Block] = {
    val (aligns, widths) = cellSpecs.unzip
    Stream(Table(caption, aligns, widths, headers, rows))
  }
  
  def simpleTable(headers: Stream[TableCell], rows: Stream[Stream[TableCell]]): Stream[Block] = {
    table(Empty, headers.map((tc: TableCell) => (AlignDefault, 0.0)), headers, rows)
  }

}