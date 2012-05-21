package pandoc.text.writers

import scala.xml.Node
import text.pandoc.Definition._
import text.pandoc.Shared.WriterOptions
import scalaz._
import Scalaz._
import scala.xml.Text
import scala.xml.NodeSeq

class HtmlWriter {
  case class WriterState(
      notes: List[Node] = Nil,
      usesMath: Boolean = false,
      usesQtag: Boolean = false,
      usesSyntaxHighlighting: Boolean = false,
      sectNum: List[Int] = Nil
  )
  
  case class HtmlTuple(
      title: Node,
      authors: List[Node],
      authsMeta: List[Node],
      date: Node,
      toc: Option[Node],
      body: Node,
      newVars: Map[String, String])
  
  /*def pandocToHtml(opts: WriterOptions, doc: Pandoc): State[WriterState, HtmlTuple] = {
    
  }*/
      
  def inlineListToHtml(opts: WriterOptions, lst: List[Inline]): State[WriterState, NodeSeq] = {
    def k(s1: State[WriterState, NodeSeq], s2: State[WriterState, Node]): State[WriteState, NodeSeq] = {
      
    }
    lst.map((in: Inline) => inlineToHtml(opts, in)).foldLeft(state((_, NodeSeq.Empty)))
  }
      
  def inlineToHtml(opts: WriterOptions, inline: Inline): State[WriterState, Node] = {
    inline match {
      case Str(str) => state((_, Text(str)))
      case Space => state((_, Text(" ")))
      case LineBreak => state((_, <br/>))
      case Emph(lst) => state((_, <em>{ inlineListToHtml(opts, lst) }</em>))
      case Strong(lst) => state((_, <strong>{ inlineListToHtml(opts, lst) }</strong>))
      
    }
  }

}