package text.pandoc.writers

import Stream.Empty
import scala.xml.{Node, NodeSeq, Text, Unparsed}
import text.pandoc.definition._
import text.pandoc.Shared.{WriterOptions, MathJax, LaTeXMathML, stringify}
import scalaz.std.stream._
import scalaz.syntax.foldable._
import scalaz._
import Scalaz._
import org.apache.commons.io.FilenameUtils
import scala.xml.{Elem, UnprefixedAttribute, MetaData}

object HtmlWriter {
  case class WriterState(
      notes: Stream[NodeSeq] = Empty,
      usesMath: Boolean = false,
      usesQtag: Boolean = false,
      usesSyntaxHighlighting: Boolean = false,
      sectNum: Stream[Int] = Empty
  )
  
  case class HtmlTuple(
      title: Node,
      authors: Stream[Node],
      authsMeta: Stream[Node],
      date: Node,
      toc: Option[Node],
      body: Node,
      newVars: Map[String, String])
      
  def nsConcat(ns: Stream[NodeSeq]): NodeSeq = ns.foldLeft(NodeSeq.Empty)(_ ++ _)
      
  
  /*def pandocToHtml(opts: WriterOptions, doc: Pandoc): State[WriterState, HtmlTuple] = {
    val (titlePrime, authorsPrime, datePrime, blocks) =
        (doc.meta.title, doc.meta.authors, doc.meta.date, doc.content)   
    for {
      standalone = opts.switches.standalone
      tit <- if (standalone) inlineListToHtml(opts, titlePrime)
    }
  }*/
      
  def nl(opts: WriterOptions): NodeSeq = if (opts.switches.wrapText) Unparsed("\n") else NodeSeq.Empty
  
  def unordList(opts: WriterOptions)(items: Stream[NodeSeq]): Elem = {
    <ul>{ nsConcat(toListItems(opts)(items)) }</ul>
  }

  def ordList(opts: WriterOptions)(items: Stream[NodeSeq]): Elem = {
    <ol>{ nsConcat(toListItems(opts)(items)) }</ol>
  }
  
  def footnoteSection(opts: WriterOptions, notes: Stream[NodeSeq]): NodeSeq = ???
  
  /*
   * 
  parseMailTo
  
  obfuscateLink
  
  def obfuscateChar
  
  def obfuscateString
   */
  
  def addAttrs(opts: WriterOptions, attr: Attr, elem: Elem): NodeSeq = elem % attrsToHtml(opts, attr)
  
  def attrsToHtml(opts: WriterOptions, attr: Attr): MetaData = {
    new UnprefixedAttribute("id", strToOpt(attr.id).map((id: String) => Text(opts.identifierPrefix + id)),
        new UnprefixedAttribute("class", strToOpt(attr.classes.mkString(" ")).map(Text(_)),
            attr.attrs.foldRight(scala.xml.Null.asInstanceOf[MetaData])((kv: KeyValue, md: MetaData) =>
              new UnprefixedAttribute(kv.key, strToOpt(kv.value).map(Text(_)), md))))
  }

  
  def isImagePath(fname: String): Boolean = {
    val imageExts = List("art", "bmp", "cdr", "cdt", "cpt", "cr2", "crw", "djvu", "erf",
              "gif", "ico", "ief", "jng", "jpg", "jpeg", "nef", "orf", "pat", "pbm",
              "pcx", "pgm", "png", "pnm", "ppm", "psd", "ras", "rgb", "svg", "tiff",
              "wbmp", "xbm", "xpm", "xwd")
    Option(FilenameUtils.getExtension(fname)).map(
        (s: String) => imageExts.contains(s.toLowerCase())).getOrElse(false)
  }
  
  def blockToHtml(opts: WriterOptions)(block: Block): State[WriterState, NodeSeq] = block match {
    case EmptyBlock => state(NodeSeq.Empty)
    case Plain(lst) => inlineListToHtml(opts)(lst)
    // TODO: handle figures for images
    //case Para(Stream(Image(txt, Target(s, Some(figTit))))) if figTit.startsWith("fig:")) => 
    //  img <- inlineToHtml(opts)(Image(txt, Target(s, Some(figTit.substring("fig:".length)))))
    case Para(lst) => for {
      contents <- inlineListToHtml(opts)(lst)  
    } yield <p>{ contents }</p>
    case RawBlock(Format("html"), str) => state(Unparsed(str))
    case RawBlock(_, _) => state(NodeSeq.Empty)
    case HorizontalRule => state(<hr/>)
    case CodeBlock(Attr(idPrime, classes, keyvals), rawCode) => 
      // TODO: highlighting of code blocks is not coded for, just putting in pre-code
      state(<pre><code>{ rawCode }</code></pre>)
    case BlockQuote(blocks) => for {
      // TODO: Not handling slideVariant in S5, whatever that is
      contents <- blockListToHtml(opts)(blocks)
    } yield <blockquote>{ nl(opts) ++ contents ++ nl(opts) }</blockquote>
    case Header(level, Attr(ident, _, _), lst) => for {
      contents <- inlineListToHtml(opts)(lst)
      // TODO: not handling section number anchors or tables of contents, yet
    } yield header(level, contents)
    case BulletList(lst) =>
      import scalaz.std.stream._
      for {
        contents <- lst.traverseS(blockListToHtml(opts))
        lstPrime = unordList(opts)(contents)
        lstDoublePrime = if (opts.switches.incremental) lstPrime % new UnprefixedAttribute("class", "incremental", scala.xml.Null) else lstPrime
      } yield lstDoublePrime
    case OrderedList(ListAttributes(startNum, numStyle, _), lst) => {
      import scalaz.std.stream._
      import scala.xml.Null
      for {
        contents <- lst.traverseS(blockListToHtml(opts))
        attribs = List[scala.xml.MetaData](
          if (opts.switches.incremental) new UnprefixedAttribute("class", "incremental", Null) else Null,
          if (startNum != 1) new UnprefixedAttribute("start", startNum.toString, Null) else Null,
          if (opts.switches.html5) html5Style(numStyle).map((s: String) => new UnprefixedAttribute("type", s, Null)).getOrElse(Null) else Null,
          if (!opts.switches.html5) cssStyle(numStyle).map((s: String) => new UnprefixedAttribute("style", "list-style-type: " + s, Null)).getOrElse(Null) else Null
        )
      } yield attribs.foldLeft(ordList(opts)(contents))((e: Elem, m: scala.xml.MetaData) => e % m)
    }
    case DefinitionList(lst) => {
      import scalaz.std.stream._
      import scala.xml.Null
      val increm = if (opts.switches.incremental) { 
        new UnprefixedAttribute("class", "incremental", Null)
      } else {
        Null
      }
      lst.traverseS((di: DefnItem) => defnItemToHtml(opts)(di)).map(nsConcat(_)).map(
          (x: NodeSeq) => <dl>{ x  ++ nl(opts) }</dl> % increm)
    }
    case Table(capt, aligns, widths, headers, rows) => {
      import scalaz.std.stream._
      def percent(w: Double) = "%d%%".format((100 * w).toInt)
      val captionDoc: State[WriterState, NodeSeq] = if (capt.isEmpty) state(NodeSeq.Empty) else for {
           cs <- inlineListToHtml(opts)(capt)
        } yield <caption>{ cs }</caption> ++ nl(opts)
      val coltags = if (widths.forall(_ == 0.0)) NodeSeq.Empty else {
          widths.map(w => if (opts.switches.html5) <col style={ "width: " + percent(w) } />
          else <col width={ percent(w) }/> ++ nl(opts))
        }
      val headPrime: State[WriterState, NodeSeq] = if (headers.forall(_.wrapped.isEmpty)) state(NodeSeq.Empty) 
          else for {
            contents <- tableRowToHtml(opts)(aligns)(0)(headers)
          } yield <thead>{ nl(opts) ++ contents }</thead> ++ nl(opts)
      val bodyPrime: State[WriterState, NodeSeq] = rows.zipWithIndex.traverseS(
          (ri: (Stream[TableCell], Int)) => tableRowToHtml(opts)(aligns)(ri._2 + 1)(ri._1)).map(
              (x: Stream[NodeSeq]) => <tbody>{ nl(opts) ++ nsConcat(x) }</tbody>)
      for {
        cd <- captionDoc
        hp <- headPrime
        bp <- bodyPrime
      } yield <table>{ nl(opts) ++ cd ++ coltags ++ hp ++ bp ++ nl(opts) }</table>
    }

  }
  
  def liftM(f: (NodeSeq => NodeSeq))(s: State[WriterState, NodeSeq]): 
      State[WriterState, NodeSeq] =  for {
    x1 <- s
  } yield f(x1)
  
  def defnItemToHtml(opts: WriterOptions)(di: DefnItem): State[WriterState, NodeSeq] = {
    import scalaz.std.stream._
    val termPrime: State[WriterState, NodeSeq] =
        if (di.term.isEmpty) State.state(NodeSeq.Empty)
        else liftM((x: NodeSeq) => nl(opts) ++ <dt>{ x }</dt> ++ nl(opts))(inlineListToHtml(opts)(di.term)) 
    val defsPrime: State[WriterState, NodeSeq] =  di.defs.traverseS((blks: Stream[Block]) => 
        liftM((x: NodeSeq) => <dd>{ x ++ nl(opts) }</dd> ++ nl(opts))(blockListToHtml(opts)(blks))).map(
        nsConcat(_))
    for {
      t <- termPrime
      d <- defsPrime
    } yield t ++ d
  }
  
  def html5Style(s: ListNumberStyle): Option[String] = s match {
    case Decimal => Some("1")
    case LowerRoman => Some("i")
    case UpperRoman => Some("I")
    case LowerAlpha => Some("a")
    case UpperAlpha => Some("A")
    case _ => None
  }
  
  def cssStyle(s: ListNumberStyle): Option[String] = s match {
    case Decimal => Some("decimal")
    case LowerRoman => Some("lower-roman")
    case UpperRoman => Some("upper-roman")
    case LowerAlpha => Some("lower-alpha")
    case UpperAlpha => Some("upper-alpha")
    case _ => None
  }

  val hs: List[(NodeSeq => NodeSeq)] = List(
    x => <h1>{ x }</h1>,
    x => <h2>{ x }</h2>,
    x => <h3>{ x }</h3>,
    x => <h4>{ x }</h4>,
    x => <h5>{ x }</h5>,
    x => <h6>{ x }</h6>    
  )
  
  def header(level: Int, contents: NodeSeq): NodeSeq = {
    if (1 <= level && level <= 6) hs(level - 1)(contents) else <p>{ contents }</p>
  }
  
  def tableRowToHtml(opts: WriterOptions)(aligns: Stream[Alignment])(rowNum: Int)(cols: Stream[TableCell]): State[WriterState, NodeSeq] =  {
    import scalaz.std.stream._
    val mkCell: (NodeSeq => Elem) = if (rowNum == 0) (x => <th>{ x }</th>) else (x => <td>{ x }</td>)
    val rowClass: String = if (rowNum == 0) "header" else if (rowNum % 2 == 0) "even" else "odd"
    for {
      colsPrime <- aligns.fzipWith(cols)((_, _)).traverseS((ai: (Alignment, TableCell)) => 
        tableItemToHtml(opts)(mkCell)(ai._1)(ai._2.wrapped))
    } yield <tr class={ rowClass }>{ nl(opts) ++ colsPrime ++ nl(opts) }</tr> 
  }
  
  def alignmentToString(align: Alignment): String = align match {
    case AlignLeft => "left"
    case AlignRight => "right"
    case AlignCenter => "center"
    case AlignDefault => "left"
  }
  
  def tableItemToHtml(opts: WriterOptions)(tagger: (NodeSeq => Elem))(align: Alignment)(item: Stream[Block]): State[WriterState, NodeSeq] = {
    for {
      contents <- blockListToHtml(opts)(item)
      alignStr = alignmentToString(align)
      attribs = if (opts.switches.html5) {
        new UnprefixedAttribute("style", "text-align: %s;".format(alignStr), scala.xml.Null)
      } else {
        new UnprefixedAttribute("align", alignStr, scala.xml.Null)
      }
    } yield (tagger(contents) % attribs) ++ nl(opts)
  }
  
  def toListItems(opts: WriterOptions)(items: Stream[NodeSeq]): Stream[NodeSeq] = {
    items.map(toListItem(opts)) :+ nl(opts)
  }
  
  def toListItem(opts: WriterOptions)(item: NodeSeq): NodeSeq = {
    nl(opts) ++ <li>{ item }</li>
  }
  
  def blockListToHtml(opts: WriterOptions)(lst: Stream[Block]): State[WriterState, NodeSeq] = {
    import scalaz.std.stream._
    lst.traverseS(blockToHtml(opts)).map(blks => nsConcat(blks.intersperse(nl(opts))))
  }
  
  def inlineListToHtml(opts: WriterOptions)(lst: Stream[Inline]): State[WriterState, NodeSeq] = {
    import scalaz.std.stream._
    lst.traverseS(inlineToHtml(opts)).map(nsConcat(_))
  }
  
  def strToOpt(str: String): Option[String] = str match {
    case null => None
    case "" => None
    case _ => Some(str)
  }
    
  def inlineToHtml(opts: WriterOptions)(inline: Inline): State[WriterState, NodeSeq] = {
    inline match {
      case Str(str) => State.state(Text(str))
      case Space => State.state(Text(" "))
      case LineBreak => State.state(<br/>)
      case Emph(lst) => inlineListToHtml(opts)(lst).map(x => <em>{ x }</em>)
      case Strong(lst) => inlineListToHtml(opts)(lst).map(x => <strong>{ x }</strong>)
      // TODO: handle code highlighting
      case Code(attr, str) => State.state(<code>{ str }</code>)
      case Strikeout(lst) => inlineListToHtml(opts)(lst).map(x => <del>{ x }</del>)
      case SmallCaps(lst) => inlineListToHtml(opts)(lst).map(x => <span style="font-variant: small-caps;">{ x }</span>)
      case Superscript(lst) => inlineListToHtml(opts)(lst).map(x => <sup>{ x }</sup>)
      case Subscript(lst) => inlineListToHtml(opts)(lst).map(x => <sub>{ x }</sub>)
      case Quoted(quoteType, lst) => {
        if (opts.switches.htmlQtags) {
          for {
            _ <- modify((s: WriterState) => s.copy(usesQtag = true))
            res <- inlineListToHtml(opts)(lst).map(x => <q>{ x }</q>)
          } yield res
        } else {
          val (leftQuote, rightQuote) = quoteType match {
            case SingleQuote => (Text("\u2018"), Text("\u2019"))  // LEFT and RIGHT SINGLE QUOTATION MARK
            case DoubleQuote => (Text("\u201C"), Text("\u201D"))  // LEFT and RIGHT DOUBLE QUOTATION MARK
          }
          inlineListToHtml(opts)(lst).map(x => leftQuote ++ x ++ rightQuote)
        }
      }
      case Math(t, str) => {
        modify((s: WriterState) => s.copy(usesMath = true)) 
        opts.htmlMathMethod match {
          case MathJax(_) => 
            val content = t match {
              case InlineMath => "\\(" + str + "\\)"
              case DisplayMath => "\\[" + str + "\\]"
            }
            state(<span class="math">{ content }</span>)
          // TODO: need to deal with other math output options
          case _ => throw new RuntimeException("only MathJax math is implemented")
        }
      }
      case RawInline(Format("latex"), str) => opts.htmlMathMethod match {
        case LaTeXMathML(_) => for {
          _ <- modify((s: WriterState) => s.copy(usesMath=true))
        } yield Text(str)
        case _ => state(NodeSeq.Empty)
      }
      case RawInline(Format("html"), str) => state(Unparsed(str))
      case RawInline(_, _) => state(NodeSeq.Empty)
      // TODO: need to obfuscate email links
      case Link(txt, Target(s, tit)) => for {
        linkText <- inlineListToHtml(opts)(txt)
      } yield <a href={ s } title={ strToOpt(tit).map(Text(_)) }>{ linkText }</a>
      case Image(txt, Target(s, tit)) if (isImagePath(s)) =>
        val alt = stringify(txt)
        val altText = if (alt != "") Some(alt) else None
        state(<img src={ s } alt={ altText.map(Text(_)) } title={ strToOpt(tit).map(Text(_)) } />)
      case Image(_, Target(s, tit)) =>
        state(<embed src={ s } title={ strToOpt(tit).map(Text(_)) } />)
      case Note(contents) => for {
    	st <- get[WriterState]
        notes = st.notes
        ref = notes.length + 1
        htmlContents <- blockListToNote(opts, ref, contents)
        _ <- put(st.copy(notes = htmlContents #:: notes))
        link = <a href={ "#%sfn%d".format(opts.identifierPrefix, ref) } class="footnoteRef" id={ "%sfnref%d".format(opts.identifierPrefix, ref) }>{ ref.toString }</a> 
        // TODO: handle EPUB3 attribute
      } yield <sup>{ link }</sup>
      case Cite(cits, il) => for {
        contents <- inlineListToHtml(opts)(il)
        citationIds = cits.map(_.id).mkString(" ")
        result = <span class="citation">{ contents }</span>
      } yield if (opts.switches.html5) result % new UnprefixedAttribute("data-cites", citationIds, scala.xml.Null) else result
    }
  }
  
  def blockListToNote(opts: WriterOptions, ref: Int, blocks: Stream[Block]): State[WriterState, NodeSeq] = {
    val backlink = Stream(Link(Stream(Str("↩")), Target("#%sfnref%d".format(opts.identifierPrefix, ref), "")))
    val blocksPrime = 
      if (blocks.isEmpty) Stream.Empty
      else {
        val lastBlock = blocks.last
        val otherBlocks = blocks.init
        lastBlock match {
          case Para(lst) => otherBlocks :+ Para(lst ++ backlink)
          case Plain(lst) => otherBlocks :+ Plain(lst ++ backlink)
          case _ => otherBlocks ++ Stream(lastBlock, Plain(backlink))
        }
      }
    for {
      contents <- blockListToHtml(opts)(blocksPrime)
      noteItem = <li id={ "%sfn%d".format(opts.identifierPrefix, ref) }>{ contents }</li>
      // TODO: EPUB3 has a special attribute for these
    } yield nl(opts) ++ noteItem
  
  }
}