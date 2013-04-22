package text.pandoc.writers

import scala.reflect.runtime.universe

import universe.{typeOf, typeTag, TypeTag}

import Stream.Empty

import java.io.PrintStream
import text.pandoc.Writer
import text.pandoc.definition._
import text.pandoc.options.WriterOptions
import text.pandoc.Pretty.{cat, char, cr, nest, render, space, text, Doc}

object Native {
  def writeNative(opts: WriterOptions, doc: Pandoc): String = {
    val colWidth: Option[Int] = if (opts.wrapText) Some(opts.columns) else None
    val blocks: Doc = prettyList(doc.content.map(prettyBlock(_)))
    val withMaybeHead = 
      if (opts.standalone) text("Pandoc (" + show(manifest[Meta])(doc.meta) + ")") %% 
          blocks %% cr
      else blocks 
    render(colWidth, withMaybeHead)
  }
  
  def prettyList(docs: Stream[Doc]): Doc = {
    char('[') <> cat(intersperse(cr <> char(','), docs.map(nest(1, _)))) <> char(']')
  }
  
  def prettyBlock(block: Block): Doc = {
    def prettifyBlockLists(blockLists: Stream[Stream[Block]]): Doc = {
      prettyList(blockLists.map((blocks: Stream[Block]) => prettyList(blocks.map(prettyBlock(_)))))
    }
    block match {
      case BlockQuote(blocks) => text("BlockQuote") %% prettyList(blocks.map(prettyBlock(_)))
      case OrderedList(attribs, blockLists) => 
        text("OrderedList") <> space <> text(show(manifest[ListAttributes])(attribs)) %% 
        prettifyBlockLists(blockLists)
      case BulletList(blockLists) => text("BulletList") %% prettifyBlockLists(blockLists)
      case DefinitionList(items) => {
        def toDoc(item: DefnItem): Doc = {
          item match {
            case DefnItem(term, defs) => text("(") <> text(
              show[Stream[Inline]](manifest[Stream[Inline]])(term)) <> 
              text(",") <> cr <>
              nest(1, prettifyBlockLists(defs)) <> text(")")
          }
        }
        text("DefinitionList") %% prettyList(items.map(toDoc(_)))
      }
      case Table(caption, aligns, widths, header, rows) => {
        text("Table ") <> text(show(manifest[Stream[Inline]])(caption)) <>
          text(" ") <> text(show(manifest[Stream[Alignment]])(aligns)) <> text(" ") <> 
          text(show(manifest[Stream[Double]])(widths)) %%
          prettifyBlockLists(header.map(_.wrapped)) %%
          prettyList(rows.map((cells: Stream[TableCell]) => prettifyBlockLists(cells.map(_.wrapped))))
      }
      case block => text(show(manifest[Block])(block))
    }
  }
  
  
  def intersperse[T](elem: T, list: Stream[T]): Stream[T] = {
    list match {
      case Empty => Empty
      case first #:: Empty => list
      case first #:: rest => first #:: elem #:: intersperse(elem, rest)
    }
  }
  
  def show[T: TypeTag](man: Manifest[T])(arg: T): String = {
    if (typeOf[T] <:< typeOf[Stream[_]]) {
      def itemType[U]: Manifest[U] = man.typeArguments(0).asInstanceOf[Manifest[U]]
      arg.asInstanceOf[Stream[_]].map(show(itemType)(_)).mkString("[", ",", "]")      
    } else {
      arg match {
      	case meta: Meta => "Meta {docTitle = %s, docAuthors = %s, docDate = %s}".format(
      	    show(manifest[Stream[Inline]])(meta.title), show(manifest[Stream[Stream[Inline]]])(meta.authors), 
      	    show(manifest[Stream[Inline]])(meta.date))
      	case block: Block => showBlock(manifest[Block])(block)
      	case inline: Inline => showInline(manifest[Inline])(inline)
      	case Attr(id, classes, attrs) => "(%s,%s,%s)".format(
      	    show(manifest[String])(id), 
      	    show(manifest[Stream[String]])(classes), show(manifest[Stream[KeyValue]])(attrs))
      	case KeyValue(key, value) => "(%s,%s)".format(show(manifest[String])(key), 
      	    show(manifest[String])(value))
      	case ListAttributes(num, style, delim) => 
      	  "(%s,%s,%s)".format(show(manifest[Int])(num), show(manifest[ListNumberStyle])(style), 
      	      show(manifest[ListNumberDelim])(delim))
      	case Target(url, title) => "(%s,%s)".format(url, title)
      	case x => x.toString
      }
    }
  }
  
  def showBlock[T: TypeTag](man: Manifest[T])(arg: T): String = {
    arg match {
      case Plain(content) => "Plain %s".format(show(manifest[Stream[Inline]])(content))
      case Para(content) => "Para %s".format(show(manifest[Stream[Inline]])(content))
      case CodeBlock(attr, str) => "CodeBlock %s %s".format(
          show(manifest[Attr])(attr), show(manifest[String])(str))
      case RawBlock(format, str) => "RawBlock %s %s".format(
          show(manifest[Format])(format), show(manifest[String])(str))
      case BlockQuote(content) => "BlockQuote %s %s".format(
          show(manifest[Stream[Block]])(content))
      case OrderedList(attrs, items) => "OrderedList %s %s".format(
          show(manifest[ListAttributes])(attrs), show(manifest[Stream[Stream[Block]]])(items))
      case BulletList(items) => "BulletList %s".format(
          show(manifest[Stream[Stream[Block]]])(items))
      case DefinitionList(items) => "DefinitionList %s".format(
          show(manifest[Stream[DefnItem]])(items))
      case Header(level, attr, content) => "Header %s %s %s".format(
          show(manifest[Int])(level), show(manifest[Attr])(attr), show(manifest[Stream[Inline]])(content))
      case HorizontalRule => "HorizontalRule"
      case Table(caption, alignments, widths, headers, rows) =>
        "Table %s %s %s %s %s".format(
            show(manifest[Stream[Inline]])(caption), 
            show(manifest[Stream[Alignment]])(alignments), 
            show(manifest[Stream[Double]])(widths), 
            show(manifest[Stream[TableCell]])(headers), 
            show(manifest[Stream[Stream[TableCell]]])(rows))
      case EmptyBlock => "[]"
    }
  }
  
  def showInline[T: TypeTag](man: Manifest[T])(arg: T): String = {
    arg match {
      case Str(str) => "Str %s".format(show(manifest[String])(str))
      case Emph(content) => "Emph %s".format(show(manifest[Stream[Inline]])(content))
      case Strong(content) => "Strong %s".format(show(manifest[Stream[Inline]])(content))
      case Strikeout(content) => "Strikeout %s".format(show(manifest[Stream[Inline]])(content))
      case Superscript(content) => "Superscript %s".format(show(manifest[Stream[Inline]])(content))
      case Subscript(content) => "Subscript %s".format(show(manifest[Stream[Inline]])(content))
      case SmallCaps(content) => "SmallCaps %s".format(show(manifest[Stream[Inline]])(content))
      case Quoted(kind, content) => "Quoted %s %s".format(
          show(manifest[QuoteType])(kind), show(manifest[Stream[Inline]])(content))
      case Cite(citations, content) => "Cite %s %s".format(
          show(manifest[Stream[Citation]])(citations), show(manifest[Stream[Inline]])(content))
      case Code(attr, str) => "Code %s %s".format(
          show(manifest[Attr])(attr), show(manifest[String])(str))
      case Space => "Space"
      case LineBreak => "LineBreak"
      case Math(kind, str) => "Math %s %s".format(
          show(manifest[MathType])(kind), show(manifest[String])(str))
      case RawInline(format, str) => "RawInline %s %s".format(
          show(manifest[Format])(format), show(manifest[String])(str))
      case Link(linkStr, target) => "Link %s %s".format(
    	  show(manifest[Stream[Inline]])(linkStr), show(manifest[Target])(target))
      case Image(altStr, target) => "Image %s %s".format(
          show(manifest[Stream[Inline]])(altStr), show(manifest[Target])(target))
      case Note(content) => "Note %s".format(show(manifest[Stream[Block]])(content))      
    }
  }
}