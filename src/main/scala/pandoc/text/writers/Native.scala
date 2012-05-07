package pandoc.text.writers

import java.io.PrintStream
import pandoc.text.Writer
import pandoc.text._
import pandoc.text.pretty.{cat, char, cr, nest, render, space, text, Doc}
import pandoc.text.Shared.WriterOptions

object Native {
  def writeNative(opts: WriterOptions, doc: Pandoc): String = {
    val colWidth: Option[Int] = if (opts.switches.wrapText) Some(opts.columns) else None
    val blocks: Doc = prettyList(doc.content.map(prettyBlock(_)))
    val withMaybeHead = 
      if (opts.switches.standalone) text("Pandoc (" + show(doc.meta) + ")") %% blocks %% cr
      else blocks 
    render(colWidth, withMaybeHead)
  }
  
  def prettyList(docs: List[Doc]): Doc = {
    char('[') <> cat(intersperse(cr <> char(','), docs.map(nest(1, _)))) <> char(']')
  }
  
  def prettyBlock(block: Block): Doc = {
    def prettifyBlockLists(blockLists: List[List[Block]]): Doc = {
      prettyList(blockLists.map((blocks: List[Block]) => prettyList(blocks.map(prettyBlock(_)))))
    }
    block match {
      case BlockQuote(blocks) => text("BlockQuote") %% prettyList(blocks.map(prettyBlock(_)))
      case OrderedList(attribs, blockLists) => 
        text("OrderedList") <> space <> text(show(attribs)) %% prettifyBlockLists(blockLists)
      case BulletList(blockLists) => text("BulletList") %% prettifyBlockLists(blockLists)
      case DefinitionList(items) => {
        def toDoc(item: DefnItem): Doc = {
          item match {
            case DefnItem(term, defs) => text("(") <> text(show(term)) <> text(",") <> cr <>
              nest(1, prettifyBlockLists(defs)) <> text(")")
          }
        }
        text("DefinitionList") %% prettyList(items.map(toDoc(_)))
      }
      case Table(caption, aligns, widths, header, rows) => {
        text("Table ") <> text(show(caption)) <>
          text(" ") <> text(show(aligns)) <> text(" ") <> text(show(widths)) %%
          prettifyBlockLists(header.map(_.wrapped)) %%
          prettyList(rows.map((cells: List[TableCell]) => prettifyBlockLists(cells.map(_.wrapped))))
      }
      case block => text(show(block))
    }
  }
  
  
  def intersperse[T](elem: T, list: List[T]): List[T] = {
    list match {
      case Nil => Nil
      case first :: Nil => list
      case first :: rest => first :: elem :: intersperse(elem, rest)
    }
  }
  
  def show[T](arg: T)(implicit man: Manifest[T]): String = {
    if (man <:< manifest[List[_]]) {
      def itemType[U]: Manifest[U] = man.typeArguments(0).asInstanceOf[Manifest[U]]
      arg.asInstanceOf[List[_]].map(show(_)(itemType)).mkString("[", ",", "]")      
    } else {
      arg match {
      	case meta: Meta => "Meta {docTitle = %s, docAuthors = %s, docDate = %s}".format(
      	    show[List[Inline]](meta.title), show[List[List[Inline]]](meta.authors), 
      	    show[List[Inline]](meta.date))
      	case block: Block => showBlock[T](arg)
      	case inline: Inline => showInline[T](arg)
      	case Attr(id, classes, attrs) => "(%s,%s,%s)".format(show(id), show(classes), show(attrs))
      	case KeyValue(key, value) => "(%s,%s)".format(show(key), show(value))
      	case ListAttributes(num, style, delim) => 
      	  "(%s,%s,%s)".format(show(num), show(style), show(delim))
      	case Target(url, title) => "(%s,%s)".format(url, title)
      	case x => x.toString
      }
    }
  }
  
  def showBlock[T](arg: T)(implicit man: Manifest[T]): String = {
    arg match {
      case Plain(content) => "Plain %s".format(show(content))
      case Para(content) => "Para %s".format(show(content))
      case CodeBlock(attr, str) => "CodeBlock %s %s".format(show(attr), show(str))
      case RawBlock(format, str) => "RawBlock %s %s".format(show(format), show(str))
      case BlockQuote(content) => "BlockQuote %s %s".format(show(content))
      case OrderedList(attrs, items) => "OrderedList %s %s".format(show(attrs), show(items))
      case BulletList(items) => "BulletList %s".format(show(items))
      case DefinitionList(items) => "DefinitionList %s".format(show(items))
      case Header(level, content) => "Header %s %s".format(show(level), show(content))
      case HorizontalRule => "HorizontalRule"
      case Table(caption, alignments, widths, headers, rows) =>
        "Table %s %s %s %s %s".format(show(caption), show(alignments), 
            show(widths), show(headers), show(rows))
      case EmptyBlock => "[]"
    }
  }
  
  def showInline[T](arg: T)(implicit man: Manifest[T]): String = {
    arg match {
      case Str(str) => "Str %s".format(show(str))
      case Emph(content) => "Emph %s".format(show(content))
      case Strong(content) => "Strong %s".format(show(content))
      case Strikeout(content) => "Strikeout %s".format(show(content))
      case Superscript(content) => "Superscript %s".format(show(content))
      case Subscript(content) => "Subscript %s".format(show(content))
      case SmallCaps(content) => "SmallCaps %s".format(show(content))
      case Quoted(kind, content) => "Quoted %s %s".format(show(kind), show(content))
      case Cite(citations, content) => "Cite %s %s".format(show(citations), show(content))
      case Code(attr, str) => "Code %s %s".format(show(attr), show(str))
      case Space => "Space"
      case LineBreak => "LineBreak"
      case Math(kind, str) => "Math %s %s".format(show(kind), show(str))
      case RawInline(format, str) => "RawInline %s %s".format(show(format), show(str))
      case Link(linkStr, target) => "Link %s %s".format(show(linkStr), show(target))
      case Image(altStr, target) => "Image %s %s".format(show(altStr), show(target))
      case Note(content) => "Note %s".format(show(content))
      
    }
  }
}




















