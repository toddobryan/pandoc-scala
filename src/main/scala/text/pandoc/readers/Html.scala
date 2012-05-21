package text.pandoc.readers

import text.tagsoup._

import text.pandoc.Parsing
import text.pandoc.Definition._
import text.pandoc.Shared._
import text.pandoc.Builder._

object Html extends Parsing {
  override type Elem = Tag
  
  def parseHeader(tags: List[Tag]): (Meta, List[Tag]) = {
    val (tit, _) = tags.dropWhile((t: Tag) => t != TagOpen("title", Nil)).drop(1).break(_ != TagClose("title"))
    val tit1 = tit.filter(_.isInstanceOf[TagText]).map {
      case TagText(txt) => txt
    }.mkString
    val tit2 = normalizeSpaces(text(tit1))
    val rest = tags.dropWhile((t: Tag) => !(t == TagClose("head") || t == TagOpen("body", Nil))).drop(1)
    (Meta(title=tit2, authors=Nil, date=Nil), rest)
  }
  
  def parseBody: Parser[List[Block]] = {
    block.* <~ eof ^^ ((lBlks: List[List[Block]]) => lBlks.flatten.map(fixPlains(false, _)))
  }
  
  def block: Parser[List[Block]] = {
    choice(List(pPara, pHeader, pBlockQuote, pCodeBlock, 
        pList, pHrule, pSimpleTable, pPlain, pRawHtmlBlock))
  }
  
  def renderTags(tags: List[Tag]): String = {
    
  }
  
  def fixPlains(inList: Boolean, blks: List[Block]): List[Block] = {
    def isParaish(b: Block): Boolean = {
      (b.isInstanceOf[Para] || b.isInstanceOf[CodeBlock] 
        || b.isInstanceOf[Header] || b.isInstanceOf[BlockQuote]) ||
      (!inList && (b.isInstanceOf[BulletList] || b.isInstanceOf[OrderedList]
                    || b.isInstanceOf[DefinitionList]))
    }
    def plainToPara(b: Block): Block = b match {
      case Plain(xs) => Para(xs)
      case x => x
    }
    if (blks.exists(isParaish(_))) blks.map(plainToPara(_))
    else blks
  }

}