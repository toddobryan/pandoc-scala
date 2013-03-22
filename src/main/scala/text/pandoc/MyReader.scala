package text.pandoc

import scala.collection.mutable
import scala.util.parsing.combinator.RegexParsers

import definition._

object MyReader extends RegexParsers {
  val normalChar = regex("""[^"\\]"""r)
  val backslashEscape = regex("""\\[abfnrtv"&'\\]"""r)
  val controlCode = regex("""\\(NUL|SOH|STX|ETX|EOT|ENQ|ACK|BEL|BS|HT|LF|VT|FF|CR|SO|SI|DLE|DC1|DC2|DC3|DC4|NAK|SYN|ETB|CAN|EM|SUB|ESC|FS|GS|RS|US|SP|DEL)"""r)
  val decimalEscape = regex("""\\\d+"""r)
  val hexEscape = regex("""\\[xX][0-9a-fA-F]+"""r)
  val octalEscape = regex("""\\[oO][0-7]+"""r)

  
  val parsers = mutable.Map[Manifest[_], () => Parser[_]](
    manifest[Int] -> (() => regex("""-?[1-9][0-9]*"""r).map(_.toInt)),
    manifest[Double] -> (() => regex("""-?([0-9]*\.[0-9]+|[0-9]+\.?)"""r).map(_.toDouble)),
    manifest[Boolean] -> (() => regex("""true|false"""r).map(_.toBoolean)),
    manifest[String] -> (() => (literal("\"") ~ rep(normalChar | backslashEscape | controlCode | decimalEscape | hexEscape | octalEscape) ~ literal("\"")).map({
      case openQ ~ chars ~ closeQ => openQ + chars.mkString + closeQ
    })))
  initializeParsers()
  
  override def skipWhitespace = false
  
  def getParser[T](implicit man: Manifest[T]): Parser[T] = {
    if (man <:< manifest[Stream[_]]) {
      val itemType = man.typeArguments(0)
      streamParser(itemType).asInstanceOf[Parser[T]]
    } else {
      parsers(man)().asInstanceOf[Parser[T]]
    }
  }
  
  def openParser(s: String): Parser[String] = {
    (regex("""\s*"""r) ~> literal(s) <~ regex("""\s*"""r))
  }
  
  val comma: Parser[String] = regex("""\s*,\s*"""r)
  
  def closeParser(s: String): Parser[String] = {
    (regex("""\s*"""r) ~> literal(s) <~ regex("""\s*"""r))
  }
  
  def streamParser[T](implicit man: Manifest[T]): Parser[Stream[T]] = {
    val itemParser = getParser(man)
    (openParser("[") ~> repsep(itemParser, comma) <~ closeParser("]")) ^^ (_.toStream)
  }
    
  def dupleParser[A, B](aParser: Parser[A], bParser: Parser[B])(implicit manA: Manifest[A], manB: Manifest[B]): Parser[(A, B)] = {
    ((openParser("(") ~> aParser <~ comma) ~ (bParser <~ closeParser(")"))).map({
      case a ~ b => (a, b)
    })
  }
  
  def tripleParser[A, B, C](aParser: Parser[A], bParser: Parser[B], cParser: Parser[C])
  		(implicit manA: Manifest[A], manB: Manifest[B], manC: Manifest[C]): Parser[(A,B,C)] = {
    ((openParser("(") ~> aParser <~ comma) ~ (bParser <~ comma) ~ (cParser <~ closeParser(")"))).map {
      case a ~ b ~ c => (a, b, c)
    }
  }
  
  def labelWithContent[T, U](label: String, resultType: Function1[Stream[T], U])(implicit man: Manifest[T]): (() => Parser[U]) = {
    () => (regex("""%s\s+""".format(label).r) ~> streamParser[T]).map(resultType.apply(_))
  }
    
  def read[T](input: String)(implicit man: Manifest[T]): ParseResult[T] = {
    parse(getParser[T], input)
  }
  
  def parsedPlusRest[T](res: ParseResult[T]): Option[(T, String)] = {
    res match {
      case Success(value, rest) => Some(value, rest.source.subSequence(res.next.offset, rest.source.length).toString)
      case _ => None
    }
  }
  
  def initializeParsers() {
    parsers += manifest[Pandoc] -> (() => (regex("""Pandoc\s+"""r) ~> getParser[Meta] ~ getParser[Stream[Block]]).map {
      case meta ~ content => Pandoc(meta, content)
    })

    parsers += manifest[Meta] -> (() => 
      ((regex("""\(\s*Meta\s+\{\s*docTitle\s*=\s*"""r) ~> getParser[Stream[Inline]] <~ regex("""\s*,\s*docAuthors\s*=\s*"""r)) ~ 
          (getParser[Stream[Stream[Inline]]] <~ regex("""\s*,\s*docDate\s*=\s*"""r)) ~
          (getParser[Stream[Inline]] <~ regex("""\s*\}\s*\)"""r))).map{
      case title ~ authors ~ date => Meta(title, authors, date)  
    })

    parsers += manifest[Alignment] -> (() => 
      literal("AlignLeft").map((s) => AlignLeft) |
      literal("AlignRight").map((s) => AlignRight) |
      literal("AlignCenter").map((s) => AlignCenter) |
      literal("AlignDefault").map((s) => AlignDefault)
    )
  
    parsers += manifest[ListAttributes] -> (() => tripleParser(getParser[Int], getParser[ListNumberStyle], getParser[ListNumberDelim]).map {
      case (num, style, delim) => ListAttributes(num, style, delim)
    })

    parsers += manifest[ListNumberStyle] -> (() =>
      literal("DefaultStyle").map((s) => DefaultStyle) |
      literal("Example").map((s) => Example) |
      literal("Decimal").map((s) => Decimal) |
      literal("LowerRoman").map((s) => LowerRoman) |
      literal("UpperRoman").map((s) => UpperRoman) |
      literal("LowerAlpha").map((s) => LowerAlpha) |
      literal("UpperAlpha").map((s) => UpperAlpha)    
    )
    
    parsers += manifest[ListNumberDelim] -> (() =>
      literal("DefaultDelim").map((s) => DefaultDelim) |
      literal("Period").map((s) => Period) |
      literal("OneParen").map((s) => OneParen) |
      literal("TwoParens").map((s) => TwoParens)
    )

    parsers += manifest[KeyValue] -> (() => dupleParser(getParser[String], getParser[String]).map{
      case (key, value) => KeyValue(key, value)
    })
    parsers += manifest[Attr] -> (() => tripleParser(getParser[String], getParser[Stream[String]], getParser[Stream[KeyValue]]).map{
      case (id, classes, attrs) => Attr(id, classes, attrs)
    })
    parsers += manifest[Plain] -> (() => (regex("""Plain\s+"""r) ~> getParser[Stream[Inline]]).map(Plain(_)))
    parsers += manifest[Para] -> (() => (regex("""Para\s+"""r) ~> getParser[Stream[Inline]]).map(Para(_)))
    parsers += manifest[CodeBlock] -> (() => (regex("""CodeBlock\s+"""r) ~> getParser[Attr] ~  getParser[String]).map {
        case attr ~ str => CodeBlock(attr, str)
      })
    parsers += manifest[RawBlock] -> (() => (regex("""RawBlock\s+"""r) ~> getParser[Format] ~ getParser[String]).map {
        case Format(format) ~ str => RawBlock(Format(format), str)
      })
    parsers += manifest[BlockQuote] -> (() => (regex("""BlockQuote\s+"""r) ~> getParser[Stream[Block]]).map(BlockQuote(_)))
    parsers += manifest[OrderedList] -> (() => (regex("""OrderedList\s+"""r) ~> getParser[ListAttributes] ~ getParser[Stream[Stream[Block]]]).map {
        case attrs ~ items => OrderedList(attrs, items)
      })
    parsers += manifest[BulletList] -> (() => (regex("""BulletList\s+"""r) ~> getParser[Stream[Stream[Block]]]).map(BulletList(_)))
    parsers += manifest[DefnItem] -> (() => dupleParser(getParser[Stream[Inline]], getParser[Stream[Stream[Block]]]).map {
      case (a, b) => DefnItem(a, b)
    })
    parsers += manifest[DefinitionList] -> (() => (regex("""DefinitionList\s+"""r) ~> getParser[Stream[DefnItem]]).map(DefinitionList(_)))
    parsers += manifest[Header] -> (() => (regex("""Header\s+"""r) ~> getParser[Int] ~ getParser[Stream[Inline]]).map{
        case level ~ content => Header(level, content)
      })
    parsers += manifest[Table] -> (() => (regex("""Table\s+"""r) ~> getParser[Stream[Inline]] ~ getParser[Stream[Alignment]] ~ getParser[Stream[Double]] ~
        getParser[Stream[TableCell]] ~ getParser[Stream[Stream[TableCell]]]).map {
          case caption ~ alignments ~ widths ~ headers ~ rows => Table(caption, alignments, widths, headers, rows)
      })
    parsers +=  manifest[Block] -> (() =>
        getParser[Plain] |
        getParser[Para] |
        getParser[CodeBlock] |
        getParser[RawBlock] |
        getParser[BlockQuote] |
        getParser[OrderedList] |
        getParser[BulletList] |
        getParser[DefinitionList] |
        getParser[Header] |
        literal("HorizontalRule").map((s) => HorizontalRule) |
        getParser[Table] |
        literal("EmptyBlock").map((s) => EmptyBlock)
      )

    parsers += manifest[QuoteType] -> (() => 
        (literal("SingleQuote").map((s) => SingleQuote) |   
         literal("DoubleQuote").map((s) => DoubleQuote)))
      
    parsers += manifest[Target] -> (() => 
        ((regex("""\(\s*"""r) ~> getParser[String]) ~ 
            (regex("""\s*,\s*"""r) ~> getParser[String] <~ regex("""\s*\)"""r))).map {
              case url ~ title => Target(url, title)
            })

    parsers += manifest[MathType] -> (() =>
        (literal("DisplayMath").map((s) => DisplayMath) | 
         literal("InlineMath").map((s) => InlineMath)))
         
    parsers += manifest[Str] -> (() => (regex("""Str\s+"""r) ~> getParser[String]).map(Str(_)))
    parsers += manifest[Emph] -> labelWithContent[Inline, Emph]("Emph", Emph(_))
    parsers += manifest[Strong] -> labelWithContent[Inline, Strong]("Strong", Strong(_))
    parsers += manifest[Strikeout] -> labelWithContent[Inline, Strikeout]("Strikeout", Strikeout(_))
    parsers += manifest[Superscript] -> labelWithContent[Inline, Superscript]("Superscript", Superscript(_))
    parsers += manifest[Subscript] -> labelWithContent[Inline, Subscript]("Subscript", Subscript(_))
    parsers += manifest[SmallCaps] -> labelWithContent[Inline, SmallCaps]("SmallCaps", SmallCaps(_))
    parsers += manifest[Quoted] -> (() => (regex("""Quoted\s+"""r) ~> getParser[QuoteType] ~ getParser[Stream[Inline]]).map {
        case kind ~ content => Quoted(kind, content)
      })
    parsers += manifest[Cite] -> (() => (regex("""Cite\s+"""r) ~> getParser[Stream[Citation]] ~ getParser[Stream[Inline]]).map {
        case citations ~ content => Cite(citations, content)
      })
    parsers += manifest[Code] -> (() => (regex("""Code\s+"""r) ~> getParser[Attr] ~ getParser[String]).map {
        case attr ~ str => Code(attr, str) 
      })
    parsers += manifest[Math] -> (() => (regex("""Math\s+"""r) ~> getParser[MathType] ~ getParser[String]).map {
        case kind ~ str => Math(kind, str)
      })
  	parsers += manifest[RawInline] -> (() => (regex("""RawInline\s+"""r) ~> getParser[Format] ~ getParser[String]).map {
  	    case format ~ str => RawInline(format, str)
  	  })
  	parsers += manifest[Link] -> (() => (regex("""Link\s+"""r) ~> getParser[Stream[Inline]] ~ getParser[Target]).map {
  	    case linkStr ~ target => Link(linkStr, target)
  	  })
  	parsers += manifest[Image] -> (() => (regex("""Image\s+"""r) ~> getParser[Stream[Inline]] ~ getParser[Target]).map {
  	    case altStr ~ target => Image(altStr, target)
  	  })
  	parsers += manifest[Note] -> (() => (regex("""Note\s+"""r) ~> getParser[Stream[Block]]).map(Note(_)))
      
    parsers += manifest[Inline] -> (() => 
        getParser[Str] |
        getParser[Emph] |
        getParser[Strong] |
        getParser[Strikeout] |
        getParser[Superscript] |
        getParser[Subscript] |
        getParser[SmallCaps] |
        getParser[Quoted] |
        getParser[Cite] |
        getParser[Code] |
        literal("Space").map((s) => Space) |
        literal("LineBreak").map((s) => LineBreak) |
        getParser[Math] |
        getParser[RawInline] |
        getParser[Link] |
        getParser[Image] |
        getParser[Note]
      )
        
    parsers += manifest[Citation] -> (() => (regex("""Citation\s+"""r) ~> getParser[String] ~ getParser[Stream[Inline]] 
          ~ getParser[Stream[Inline]] ~ getParser[CitationMode] ~ getParser[Int] ~ getParser[Int]).map {
            case id ~ prefix ~ suffix ~ mode ~ noteNum ~ hash => Citation(id, prefix, suffix, mode, noteNum, hash) 
        })
      
    parsers += manifest[CitationMode] -> (() =>
        (literal("AuthorInText").map((s) => AuthorInText) | 
         literal("SuppressAuthor").map((s) => SuppressAuthor) | 
         literal("NormalCitation").map((s) => NormalCitation)))
  }

}
