package text.pandoc

import Definition._

object Builder {
  def doc(blocks: List[Block]): Pandoc = Pandoc(Meta(Nil, Nil, Nil), blocks)
  
  def setTitle(t: List[Inline], doc: Pandoc): Pandoc = {
    doc.copy(meta=doc.meta.copy(title=t))
  }
  
  def setAuthors(as: List[List[Inline]], doc: Pandoc): Pandoc = {
    doc.copy(meta=doc.meta.copy(authors=as))
  }
  
  def setDate(d: List[Inline], doc: Pandoc): Pandoc = {
    doc.copy(meta=doc.meta.copy(date=d))
  }
  
  def text(str: String): List[Inline] = {
    def isSpace(c: Char): Boolean = " \n\t".contains(c)
    def helper(cs: List[Char]): List[Inline] = cs match {
      case Nil => Nil
      case x :: xs if isSpace(x) => Space :: helper(xs.dropWhile(isSpace _))
      case x => {
        val (s, rest) = x.span(!isSpace(_))
        Str(s.mkString) :: helper(rest)
      }
    }
    helper(str.toList)
  }
  
  def str(s: String): List[Inline] = List(Str(s))
  
  def emph(ins: List[Inline]): List[Inline] = List(Emph(ins))
  
  def strong(ins: List[Inline]): List[Inline] = List(Strong(ins))
  
  def strikeout(ins: List[Inline]): List[Inline] = List(Strikeout(ins))
  
  def superscript(ins: List[Inline]): List[Inline] = List(Superscript(ins))
  
  def subscript(ins: List[Inline]): List[Inline] = List(Subscript(ins))
  
  def smallCaps(ins: List[Inline]): List[Inline] = List(SmallCaps(ins))
  
  def singleQuoted(ins: List[Inline]): List[Inline] = quoted(SingleQuote, ins)
  
  def doubleQuoted(ins: List[Inline]): List[Inline] = quoted(DoubleQuote, ins)
  
  def quoted(typ: QuoteType, ins: List[Inline]): List[Inline] = List(Quoted(typ, ins))
  
  def cite(cts: List[Citation], ins: List[Inline]): List[Inline] = List(Cite(cts, ins))
  
  def codeWith(attrs: Attr, str: String): List[Inline] = List(Code(attrs, str))
  
  def code(str: String): List[Inline] = codeWith(NullAttr, str)
  
  def space: List[Inline] = List(Space)
  
  def lineBreak: List[Inline] = List(LineBreak)
  
  def math(str: String): List[Inline] = List(Math(InlineMath, str))
  
  def displayMath(str: String): List[Inline] = List(Math(DisplayMath, str))
  
  def rawInline(format: Format, str: String): List[Inline] = List(RawInline(format, str))
  
  def link(url: String, title: String, label: List[Inline]): List[Inline] = {
    List(Link(label, Target(url, title)))
  }
  
  def image(url: String, title: String, alt: List[Inline]): List[Inline] = {
    List(Image(alt, Target(url, title)))
  }
  
  def note(blks: List[Block]): List[Inline] = List(Note(blks))
  
  def para(ins: List[Inline]): List[Block] = List(Para(ins))
  
  def plain(ins: List[Inline]): List[Block] = List(Plain(ins))
  
  def codeBlockWith(attrs: Attr, str: String): List[Block] = {
    List(CodeBlock(attrs, str))
  }
  
  def codeBlock(str: String): List[Block] = codeBlockWith(NullAttr, str)
  
  def rawBlock(format: Format, str: String): List[Block] = List(RawBlock(format, str))
  
  def blockQuote(blks: List[Block]): List[Block] = {
    List(BlockQuote(blks))
  }
  
  def orderedListWith(attrs: ListAttributes, items: List[List[Block]]): List[Block] = {
    List(OrderedList(attrs, items))
  }
  
  def orderedList(items: List[List[Block]]): List[Block] = {
    orderedListWith(ListAttributes(1, DefaultStyle, DefaultDelim), items)
  }
  
  def bulletList(items: List[List[Block]]): List[Block] = List(BulletList(items))
  
  def definitionList(items: List[DefnItem]): List[Block] = {
    List(DefinitionList(items))
  }
  
  def header(level: Int, ins: List[Inline]): List[Block] = {
    List(Header(level, ins))
  }
  
  def horizontalRule: List[Block] = List(HorizontalRule)
  
  def table(caption: List[Inline], 
            cellSpecs: List[(Alignment, Double)],
            headers: List[TableCell],
            rows: List[List[TableCell]]): List[Block] = {
    val (aligns, widths) = cellSpecs.unzip
    List(Table(caption, aligns, widths, headers, rows))
  }
  
  def simpleTable(headers: List[TableCell], rows: List[List[TableCell]]): List[Block] = {
    table(Nil, headers.map((tc: TableCell) => (AlignDefault, 0.0)), headers, rows)
  }

}