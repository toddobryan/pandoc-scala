package text.pandoc.readers

import Stream.Empty

import text.tagsoup._

import text.pandoc.Parsing
import text.pandoc.definition._
import text.pandoc.Shared._
import text.pandoc.Builder._

/*object Html extends Parsing {
  override type Elem = Tag
  
  def parseHeader(tags: Stream[Tag]): (Meta, Stream[Tag]) = {
    val (tit, _) = tags.dropWhile((t: Tag) => t != TagOpen("title", Nil)).drop(1).span(_ == TagClose("title"))
    val tit1 = tit.filter(_.isInstanceOf[TagText]).map {
      case TagText(txt) => txt
    }.mkString
    val tit2 = normalizeSpaces(text(tit1))
    val rest = tags.dropWhile((t: Tag) => !(t == TagClose("head") || t == TagOpen("body", Nil))).drop(1)
    (Meta(title=tit2, authors=Empty, date=Empty), rest)
  }
  
  def parseBody: Parser[Stream[Block]] = {
    (block.* <~ eof) ^^ (_.toStream.flatMap(fixPlains(false, _)))
  }
  
  def block: Parser[Stream[Block]] = {
    choice(Stream(pPara, pHeader, pBlockQuote, pCodeBlock, 
        pList, pHrule, pSimpleTable, pPlain, pRawHtmlBlock))
  }
  
  def renderTags(tags: Stream[Tag]): String = {
    
  }
  
  def fixPlains(inList: Boolean, blks: Stream[Block]): List[Block] = {
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

}*/