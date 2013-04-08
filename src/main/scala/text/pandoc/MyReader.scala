package text.pandoc

import scala.reflect.runtime.universe
import scala.util.parsing.combinator.RegexParsers

import universe.{typeOf, typeRef, TypeRef, typeTag, TypeTag}

import definition._

object MyReader extends RegexParsers {
  val normalChar = regex("""[^"\\]""".r)
  val backslashEscape = regex("""\\[abfnrtv"&'\\]""".r)
  val controlCode = regex("""\\(NUL|SOH|STX|ETX|EOT|ENQ|ACK|BEL|BS|HT|LF|VT|FF|CR|SO|SI|DLE|DC1|DC2|DC3|DC4|NAK|SYN|ETB|CAN|EM|SUB|ESC|FS|GS|RS|US|SP|DEL)""".r)
  val decimalEscape = regex("""\\\d+""".r)
  val hexEscape = regex("""\\[xX][0-9a-fA-F]+""".r)
  val octalEscape = regex("""\\[oO][0-7]+""".r)
  
  override def skipWhitespace = false
  
  def parsers[T: TypeTag](man: Manifest[T]): Parser[T] = (typeOf[T] match {
    case t if t =:= typeOf[Int] => regex("""-?[1-9][0-9]*""".r).map(_.toInt)
    case t if t =:= typeOf[Double] => regex("""-?([0-9]*\.[0-9]+|[0-9]+\.?)""".r).map(_.toDouble)
    case t if t =:= typeOf[Boolean] => regex("""true|false""".r).map(_.toBoolean)
    case t if t =:= typeOf[String] => (literal("\"") ~ rep(normalChar | backslashEscape | controlCode | decimalEscape | hexEscape | octalEscape) ~ literal("\"")).map({
      case openQ ~ chars ~ closeQ => openQ + chars.mkString + closeQ
    })
    case t if t =:= typeOf[Pandoc] => (regex("""Pandoc\s+""".r) ~> getParser(manifest[Meta]) ~ getParser(manifest[Stream[Block]])).map {
      case meta ~ content => Pandoc(meta, content)
    }
    case t if t =:= typeOf[Meta] => 
      ((regex("""\(\s*Meta\s+\{\s*docTitle\s*=\s*""".r) ~> getParser(manifest[Stream[Inline]]) <~ regex("""\s*,\s*docAuthors\s*=\s*""".r)) ~ 
          (getParser(manifest[Stream[Stream[Inline]]]) <~ regex("""\s*,\s*docDate\s*=\s*""".r)) ~
          (getParser(manifest[Stream[Inline]]) <~ regex("""\s*\}\s*\)""".r))).map{
      case title ~ authors ~ date => Meta(title, authors, date)  
    }
    case t if t =:= typeOf[Alignment] => 
      literal("AlignLeft").map((s) => AlignLeft) |
      literal("AlignRight").map((s) => AlignRight) |
      literal("AlignCenter").map((s) => AlignCenter) |
      literal("AlignDefault").map((s) => AlignDefault)
    case t if t =:= typeOf[ListAttributes] => tripleParser(getParser(manifest[Int]), getParser(manifest[ListNumberStyle]), getParser(manifest[ListNumberDelim])).map {
      case (num, style, delim) => ListAttributes(num, style, delim)
    }
    case t if t =:= typeOf[ListNumberStyle] =>
      literal("DefaultStyle").map((s) => DefaultStyle) |
      literal("Example").map((s) => Example) |
      literal("Decimal").map((s) => Decimal) |
      literal("LowerRoman").map((s) => LowerRoman) |
      literal("UpperRoman").map((s) => UpperRoman) |
      literal("LowerAlpha").map((s) => LowerAlpha) |
      literal("UpperAlpha").map((s) => UpperAlpha)    
    case t if t =:= typeOf [ListNumberDelim] =>
      literal("DefaultDelim").map((s) => DefaultDelim) |
      literal("Period").map((s) => Period) |
      literal("OneParen").map((s) => OneParen) |
      literal("TwoParens").map((s) => TwoParens)
    case t if t =:= typeOf[KeyValue] => dupleParser(getParser(manifest[String]), getParser(manifest[String])).map{
      case (key, value) => KeyValue(key, value)
    }
    case t if t =:= typeOf[Attr] => tripleParser(getParser(manifest[String]), getParser(manifest[Stream[String]]), getParser(manifest[Stream[KeyValue]])).map{
      case (id, classes, attrs) => Attr(id, classes, attrs)
    }
    case t if t =:= typeOf[Plain] => (regex("""Plain\s+""".r) ~> getParser(manifest[Stream[Inline]])).map(Plain(_))
    case t if t =:= typeOf[Para] => (regex("""Para\s+""".r) ~> getParser(manifest[Stream[Inline]])).map(Para(_))
    case t if t =:= typeOf[CodeBlock] => (regex("""CodeBlock\s+""".r) ~> getParser(manifest[Attr]) ~  getParser(manifest[String])).map {
        case attr ~ str => CodeBlock(attr, str)
    }
    case t if t =:= typeOf[RawBlock] => (regex("""RawBlock\s+""".r) ~> getParser(manifest[Format]) ~ getParser(manifest[String])).map {
        case Format(format) ~ str => RawBlock(Format(format), str)
    }
    case t if t =:= typeOf[BlockQuote] => (regex("""BlockQuote\s+""".r) ~> getParser(manifest[Stream[Block]])).map(BlockQuote(_))
    case t if t =:= typeOf[OrderedList] => (regex("""OrderedList\s+""".r) ~> getParser(manifest[ListAttributes]) ~ getParser(manifest[Stream[Stream[Block]]])).map {
        case attrs ~ items => OrderedList(attrs, items)
    }
    case t if t =:= typeOf[BulletList] => (regex("""BulletList\s+""".r) ~> getParser(manifest[Stream[Stream[Block]]])).map(BulletList(_))
    case t if t =:= typeOf[DefnItem] => dupleParser(getParser(manifest[Stream[Inline]]), getParser(manifest[Stream[Stream[Block]]])).map {
      case (a, b) => DefnItem(a, b)
    }
    case t if t =:= typeOf[DefinitionList] => (regex("""DefinitionList\s+""".r) ~> getParser(manifest[Stream[DefnItem]])).map(DefinitionList(_))
    case t if t =:= typeOf[Header] => (regex("""Header\s+""".r) ~> getParser(manifest[Int]) ~ getParser(manifest[Attr]) ~ getParser(manifest[Stream[Inline]])).map{
        case level ~ attr ~ content => Header(level, attr, content)
    }
    case t if t =:= typeOf[Table] => (regex("""Table\s+""".r) ~> getParser(manifest[Stream[Inline]]) ~ getParser(manifest[Stream[Alignment]]) ~ getParser(manifest[Stream[Double]]) ~
        getParser(manifest[Stream[TableCell]]) ~ getParser(manifest[Stream[Stream[TableCell]]])).map {
          case caption ~ alignments ~ widths ~ headers ~ rows => Table(caption, alignments, widths, headers, rows)
    }
    case t if t =:= typeOf[Block] =>
        getParser(manifest[Plain]) |
        getParser(manifest[Para]) |
        getParser(manifest[CodeBlock]) |
        getParser(manifest[RawBlock]) |
        getParser(manifest[BlockQuote]) |
        getParser(manifest[OrderedList]) |
        getParser(manifest[BulletList]) |
        getParser(manifest[DefinitionList]) |
        getParser(manifest[Header]) |
        literal("HorizontalRule").map((s) => HorizontalRule) |
        getParser(manifest[Table]) |
        literal("EmptyBlock").map((s) => EmptyBlock)
    case t if t =:= typeOf[QuoteType] => 
        (literal("SingleQuote").map((s) => SingleQuote) |   
         literal("DoubleQuote").map((s) => DoubleQuote))
    case t if t =:= typeOf[Target] => 
        ((regex("""\(\s*""".r) ~> getParser(manifest[String])) ~ 
            (regex("""\s*,\s*""".r) ~> getParser(manifest[String]) <~ regex("""\s*\)""".r))).map {
              case url ~ title => Target(url, title)
            }
    case t if t =:= typeOf[MathType] =>
        (literal("DisplayMath").map((s) => DisplayMath) | 
         literal("InlineMath").map((s) => InlineMath))
    case t if t =:= typeOf[Str] => (regex("""Str\s+""".r) ~> getParser(manifest[String])).map(Str(_))
    case t if t =:= typeOf[Emph] => labelWithContent[Inline, Emph]("Emph", Emph(_))
    case t if t =:= typeOf[Strong] => labelWithContent[Inline, Strong]("Strong", Strong(_))
    case t if t =:= typeOf[Strikeout] => labelWithContent[Inline, Strikeout]("Strikeout", Strikeout(_))
    case t if t =:= typeOf[Superscript] => labelWithContent[Inline, Superscript]("Superscript", Superscript(_))
    case t if t =:= typeOf[Subscript] => labelWithContent[Inline, Subscript]("Subscript", Subscript(_))
    case t if t =:= typeOf[SmallCaps] => labelWithContent[Inline, SmallCaps]("SmallCaps", SmallCaps(_))
    case t if t =:= typeOf[Quoted] => (regex("""Quoted\s+""".r) ~> getParser(manifest[QuoteType]) ~ getParser(manifest[Stream[Inline]])).map {
        case kind ~ content => Quoted(kind, content)
    }
    case t if t =:= typeOf[Cite] => (regex("""Cite\s+""".r) ~> getParser(manifest[Stream[Citation]]) ~ getParser(manifest[Stream[Inline]])).map {
        case citations ~ content => Cite(citations, content)
    }
    case t if t =:= typeOf[Code] => (regex("""Code\s+""".r) ~> getParser(manifest[Attr]) ~ getParser(manifest[String])).map {
        case attr ~ str => Code(attr, str) 
    }
    case t if t =:= typeOf[Math] => (regex("""Math\s+""".r) ~> getParser(manifest[MathType]) ~ getParser(manifest[String])).map {
        case kind ~ str => Math(kind, str)
    }
  	case t if t =:= typeOf[RawInline] => (regex("""RawInline\s+""".r) ~> getParser(manifest[Format]) ~ getParser(manifest[String])).map {
  	    case format ~ str => RawInline(format, str)
  	}
  	case t if t =:= typeOf[Link] => (regex("""Link\s+""".r) ~> getParser(manifest[Stream[Inline]]) ~ getParser(manifest[Target])).map {
  	    case linkStr ~ target => Link(linkStr, target)
  	}
  	case t if t =:= typeOf[Image] => (regex("""Image\s+""".r) ~> getParser(manifest[Stream[Inline]]) ~ getParser(manifest[Target])).map {
  	    case altStr ~ target => Image(altStr, target)
  	}
  	case t if t =:= typeOf[Note] => (regex("""Note\s+""".r) ~> getParser(manifest[Stream[Block]])).map(Note(_))
      
    case t if t =:= typeOf[Inline] => 
        getParser(manifest[Str]) |
        getParser(manifest[Emph]) |
        getParser(manifest[Strong]) |
        getParser(manifest[Strikeout]) |
        getParser(manifest[Superscript]) |
        getParser(manifest[Subscript]) |
        getParser(manifest[SmallCaps]) |
        getParser(manifest[Quoted]) |
        getParser(manifest[Cite]) |
        getParser(manifest[Code]) |
        literal("Space").map((s) => Space) |
        literal("LineBreak").map((s) => LineBreak) |
        getParser(manifest[Math]) |
        getParser(manifest[RawInline]) |
        getParser(manifest[Link]) |
        getParser(manifest[Image]) |
        getParser(manifest[Note])        
    case t if t =:= typeOf[Citation] => (regex("""Citation\s+""".r) ~> getParser(manifest[String]) ~ getParser(manifest[Stream[Inline]]) 
          ~ getParser(manifest[Stream[Inline]]) ~ getParser(manifest[CitationMode]) ~ getParser(manifest[Int]) ~ getParser(manifest[Int])).map {
            case id ~ prefix ~ suffix ~ mode ~ noteNum ~ hash => Citation(id, prefix, suffix, mode, noteNum, hash) 
    }
    case t if t =:= typeOf[CitationMode] =>
        (literal("AuthorInText").map((s) => AuthorInText) | 
         literal("SuppressAuthor").map((s) => SuppressAuthor) | 
         literal("NormalCitation").map((s) => NormalCitation))
  }).asInstanceOf[Parser[T]]
  
  def getParser[T: TypeTag](man: Manifest[T]): Parser[T] = {
    if (typeOf[T] <:< typeOf[Stream[_]]) {
      val itemType = man.typeArguments(0)
      streamParser(itemType).asInstanceOf[Parser[T]]
    } else {
      parsers(man).asInstanceOf[Parser[T]]
    }
  }

  def streamParser[T](implicit man: Manifest[T]): Parser[Stream[T]] = {
    val itemParser = getParser(man)
    (openParser("[") ~> repsep(itemParser, comma) <~ closeParser("]")) ^^ (_.toStream)
  }
  
  def optionParser[T](implicit man: Manifest[T]): Parser[Option[T]] = {
    val itemParser = getParser(man)
    "None" ^^^ None | ((openParser("Some(") ~> itemParser <~ closeParser(")")) ^^ ((t: T) => Some(t)))
  }
  
  def openParser(s: String): Parser[String] = {
    (regex("""\s*""".r) ~> literal(s) <~ regex("""\s*""".r))
  }
  
  val comma: Parser[String] = regex("""\s*,\s*""".r)
  
  def closeParser(s: String): Parser[String] = {
    (regex("""\s*""".r) ~> literal(s) <~ regex("""\s*""".r))
  }
      
  def dupleParser[A: TypeTag, B: TypeTag](aParser: Parser[A], bParser: Parser[B]): Parser[(A, B)] = {
    ((openParser("(") ~> aParser <~ comma) ~ (bParser <~ closeParser(")"))).map({
      case a ~ b => (a, b)
    })
  }
  
  def tripleParser[A: TypeTag, B: TypeTag, C: TypeTag](aParser: Parser[A], bParser: Parser[B], cParser: Parser[C]): Parser[(A,B,C)] = {
    ((openParser("(") ~> aParser <~ comma) ~ (bParser <~ comma) ~ (cParser <~ closeParser(")"))).map {
      case a ~ b ~ c => (a, b, c)
    }
  }
  
  def labelWithContent[T: TypeTag, U](label: String, resultType: Function1[Stream[T], U])(implicit man: Manifest[T]): Parser[U] = {
    (regex("""%s\s+""".format(label).r) ~> streamParser[T](man)).map(resultType.apply(_))
  }
    
  def read[T: TypeTag](input: String)(implicit man: Manifest[T]): ParseResult[T] = {
    parse(getParser[T](man), input)
  }
  
  def parsedPlusRest[T](res: ParseResult[T]): Option[(T, String)] = {
    res match {
      case Success(value, rest) => Some(value, rest.source.subSequence(res.next.offset, rest.source.length).toString)
      case _ => None
    }
  }
}
