package pandoc.text.writers

import Stream.Empty
import scala.xml.{Node, NodeSeq, Text}

import text.pandoc.definition._
import text.pandoc.Shared.WriterOptions
import scalaz._
import Scalaz._


class HtmlWriter {
  case class WriterState(
      notes: Stream[Node] = Empty,
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
  
  /*def pandocToHtml(opts: WriterOptions, doc: Pandoc): State[WriterState, HtmlTuple] = {
    val (titlePrime, authorsPrime, datePrime, blocks) =
        (doc.meta.title, doc.meta.authors, doc.meta.date, doc.content)   
    for {
      standalone = opts.switches.standalone
      tit <- if (standalone) inlineListToHtml(opts, titlePrime)
    }
  }*/
      
  /*def inlineListToHtml(opts: WriterOptions, lst: Stream[Inline]): State[WriterState, NodeSeq] = {
    mapM(inlineToHtml(opts)) 
    
  }*/
      
  def inlineToHtml(opts: WriterOptions)(inline: Inline): State[WriterState, Node] = {
    inline match {
      case Str(str) => state((_, Text(str)))
      case Space => state((_, Text(" ")))
      //case LineBreak => state((_, <br/>))
      //case Emph(lst) => state((_, <em>{ inlineListToHtml(opts, lst) }</em>))
      //case Strong(lst) => state((_, <strong>{ inlineListToHtml(opts, lst) }</strong>))
      
    }
  }

}