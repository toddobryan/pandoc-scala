package pandoc.text

package object pretty {
  case class RenderState[T](
      output: Seq[T],
      prefix: String,
      usePrefix: Boolean,
      lineLength: Option[Int],
      column: Int,
      newLines: Int
  )
  
  sealed abstract class D {
    def isBlank: Boolean = false
  }
  case class Text(len: Int, content: String) extends D {
    override def isBlank: Boolean = content.length > 0 && content.charAt(0).isSpaceChar
  }
  case class Block(width: Int, content: List[String]) extends D
  case class Prefixed(prefix: String, doc: Doc) extends D
  case class BeforeNonBlank(doc: Doc) extends D
  case class Flush(doc: Doc) extends D
  case class Blank extends D {
    override def isBlank: Boolean = true
  }
  case object BreakingSpace extends Blank
  case object CarriageReturn extends Blank
  case object NewLine extends Blank
  case object BlankLine extends Blank
  
  case class Doc(content: List[D]) {
    def isEmpty: Boolean = content.isEmpty
    def <>(that: Doc): Doc = Doc(this.content ++ that.content)
    def <+>(that: Doc): Doc = {
      if (this.isEmpty) that
      else if (that.isEmpty) this
      else this <> space <> that
    }
    def %%(that: Doc): Doc = { // Haskell's $$
      if (this.isEmpty) that
      else if (that.isEmpty) this
      else this <> cr <> that
    }
    def %+%(that: Doc): Doc = { // Haskell's $+$
      if (this.isEmpty) that
      else if (that.isEmpty) this
      else this <> blankline <> that
    }
    def chomp: Doc = {
      Doc(this.content.reverse.dropWhile(_.isBlank).reverse)
    }
  }
  
  def cat(docs: List[Doc]): Doc = Doc(docs.flatMap(_.content))
  def hcat = cat _
  def hsep(docs: List[Doc]): Doc = docs.foldRight[Doc](empty)((l: Doc, r: Doc) => l <+> r)
  def vcat(docs: List[Doc]): Doc = docs.foldRight[Doc](empty)((l: Doc, r: Doc) => l %% r)
  def vsep(docs: List[Doc]): Doc = docs.foldRight[Doc](empty)((l: Doc, r: Doc) => l %+% r)

  def empty = Doc(Nil)
  def toChunks(s: List[Char]): List[D] = {
    s match {
      case Nil => Nil
      case x => x.span(_ != '\n') match {
        case (Nil, _ :: ys) => NewLine +: toChunks(ys)
        case (xs, _ :: ys) => Text(realLength(xs), xs.mkString) +: NewLine +: toChunks(ys)
        case (xs, Nil) => List(Text(realLength(xs), xs.mkString))
      }
    }
  }
  def text(s: String): Doc = {
    Doc(toChunks(s.toCharArray.toList))
  }
  def char(c: Char): Doc = text(c.toString)
  def space = Doc(List(BreakingSpace))
  def cr = Doc(List(CarriageReturn))
  def blankline = Doc(List(BlankLine))

/*
-- | A literal string.
text :: String -> Doc
text = Doc . toChunks
  where toChunks :: String -> Seq D
        toChunks [] = mempty
        toChunks s = case break (=='\n') s of
                          ([], _:ys) -> NewLine <| toChunks ys
                          (xs, _:ys) -> Text (realLength xs) xs <|
                                            (NewLine <| toChunks ys)
                          (xs, [])      -> singleton $ Text (realLength xs) xs

-- | A character.
char :: Char -> Doc
char c = text [c]

-- | A breaking (reflowable) space.
space :: Doc
space = Doc $ singleton BreakingSpace

-- | A carriage return.  Does nothing if we're at the beginning of
-- a line; otherwise inserts a newline.
cr :: Doc
cr = Doc $ singleton CarriageReturn

-- | Inserts a blank line unless one exists already.
-- (@blankline <> blankline@ has the same effect as @blankline@.
-- If you want multiple blank lines, use @text "\\n\\n"@.
blankline :: Doc
blankline = Doc $ singleton BlankLine

-- | Uses the specified string as a prefix for every line of
-- the inside document (except the first, if not at the beginning
-- of the line).
prefixed :: String -> Doc -> Doc
prefixed pref doc = Doc $ singleton $ Prefixed pref doc

-- | Makes a 'Doc' flush against the left margin.
flush :: Doc -> Doc
flush doc = Doc $ singleton $ Flush doc

-- | Indents a 'Doc' by the specified number of spaces.
nest :: Int -> Doc -> Doc
nest ind = prefixed (replicate ind ' ')

-- | A hanging indent. @hang ind start doc@ prints @start@,
-- then @doc@, leaving an indent of @ind@ spaces on every
-- line but the first.
hang :: Int -> Doc -> Doc -> Doc
hang ind start doc = start <> nest ind doc

-- | @beforeNonBlank d@ conditionally includes @d@ unless it is
-- followed by blank space.
beforeNonBlank :: Doc -> Doc
beforeNonBlank d = Doc $ singleton (BeforeNonBlank d)

-- | Makes a 'Doc' non-reflowable.
nowrap :: Doc -> Doc
nowrap doc = Doc $ mapWithIndex replaceSpace $ unDoc doc
  where replaceSpace _ BreakingSpace = Text 1 " "
        replaceSpace _ x = x

-- | Returns the width of a 'Doc'.
offset :: Doc -> Int
offset d = case map realLength . lines . render Nothing $ d of
                []    -> 0
                os    -> maximum os

block :: (String -> String) -> Int -> Doc -> Doc
block filler width = Doc . singleton . Block width .
                      map filler . chop width . render (Just width)

-- | @lblock n d@ is a block of width @n@ characters, with
-- text derived from @d@ and aligned to the left.
lblock :: Int -> Doc -> Doc
lblock = block id

-- | Like 'lblock' but aligned to the right.
rblock :: Int -> Doc -> Doc
rblock w = block (\s -> replicate (w - realLength s) ' ' ++ s) w

-- | Like 'lblock' but aligned centered.
cblock :: Int -> Doc -> Doc
cblock w = block (\s -> replicate ((w - realLength s) `div` 2) ' ' ++ s) w

-- | Returns the height of a block or other 'Doc'.
height :: Doc -> Int
height = length . lines . render Nothing

chop :: Int -> String -> [String]
chop _ [] = []
chop n cs = case break (=='\n') cs of
                  (xs, ys)     -> if len <= n
                                     then case ys of
                                             []     -> [xs]
                                             (_:[]) -> [xs, ""]
                                             (_:zs) -> xs : chop n zs
                                     else take n xs : chop n (drop n xs ++ ys)
                                   where len = realLength xs
*/
  
  def inside(start: Doc, end: Doc)(contents: Doc): Doc = start <> contents <> end
  def braces(doc: Doc): Doc = inside(char('{'), char('}'))(doc)
  
  
/*
inside :: Doc -> Doc -> Doc -> Doc
inside start end contents =
  start <> contents <> end

-- | Puts a 'Doc' in curly braces.
braces :: Doc -> Doc
braces = inside (char '{') (char '}')

-- | Puts a 'Doc' in square brackets.
brackets :: Doc -> Doc
brackets = inside (char '[') (char ']')

-- | Puts a 'Doc' in parentheses.
parens :: Doc -> Doc
parens = inside (char '(') (char ')')

-- | Wraps a 'Doc' in single quotes.
quotes :: Doc -> Doc
quotes = inside (char '\'') (char '\'')

-- | Wraps a 'Doc' in double quotes.
doubleQuotes :: Doc -> Doc
doubleQuotes = inside (char '"') (char '"')
*/
  
  def charWidth(c: Char): Int = {
    if (c < 0x0300) 1
    else if (c >= 0x0300 && c <= 0x036f) 0
    else if (c >= 0x0370 && c <= 0x10FC) 1
    else if (c >= 0x1100 && c <= 0x115F) 2
    else if (c >= 0x1160 && c <= 0x11A2) 1
    else if (c >= 0x11A3 && c <= 0x11A7) 2
    else if (c >= 0x11A8 && c <= 0x11F9) 1
    else if (c >= 0x11FA && c <= 0x11FF) 2
    else if (c >= 0x1200 && c <= 0x2328) 1
    else if (c >= 0x2329 && c <= 0x232A) 2
    else if (c >= 0x232B && c <= 0x2E31) 1
    else if (c >= 0x2E80 && c <= 0x303E) 2
    else if (c == 0x303F) 1
    else if (c >= 0x3041 && c <= 0x3247) 2
    else if (c >= 0x3248 && c <= 0x324F) 1 // ambiguous
    else if (c >= 0x3250 && c <= 0x4DBF) 2
    else if (c >= 0x4DC0 && c <= 0x4DFF) 1
    else if (c >= 0x4E00 && c <= 0xA4C6) 2
    else if (c >= 0xA4D0 && c <= 0xA95F) 1
    else if (c >= 0xA960 && c <= 0xA97C) 2
    else if (c >= 0xA980 && c <= 0xABF9) 1
    else if (c >= 0xAC00 && c <= 0xD7FB) 2
    else if (c >= 0xD800 && c <= 0xDFFF) 1
    else if (c >= 0xE000 && c <= 0xF8FF) 1 // ambiguous
    else if (c >= 0xF900 && c <= 0xFAFF) 2
    else if (c >= 0xFB00 && c <= 0xFDFD) 1
    else if (c >= 0xFE00 && c <= 0xFE0F) 1 // ambiguous
    else if (c >= 0xFE10 && c <= 0xFE19) 2
    else if (c >= 0xFE20 && c <= 0xFE26) 1
    else if (c >= 0xFE30 && c <= 0xFE6B) 2
    else if (c >= 0xFE70 && c <= 0x16A38) 1
    else if (c >= 0x1B000 && c <= 0x1B001) 2
    else if (c >= 0x1D000 && c <= 0x1F1FF) 1
    else if (c >= 0x1F200 && c <= 0x1F251) 2
    else if (c >= 0x1F300 && c <= 0x1F773) 1
    else if (c >= 0x20000 && c <= 0x3FFFD) 2
    else 1
  }
  
  def realLength(s: List[Char]): Int = {
    s.map(charWidth _).foldLeft[Int](0)((l: Int, r: Int) => l + r)
  }
}
/*

outp :: (IsString a, Monoid a)
     => Int -> String -> DocState a
outp off s | off <= 0 = do
  st' <- get
  let rawpref = prefix st'
  when (column st' == 0 && usePrefix st' && not (null rawpref)) $ do
    let pref = reverse $ dropWhile isSpace $ reverse rawpref
    modify $ \st -> st{ output = fromString pref : output st
                      , column = column st + realLength pref }
  when (off < 0) $ do
     modify $ \st -> st { output = fromString s : output st
                        , column = 0
                        , newlines = newlines st + 1 }
outp off s = do
  st' <- get
  let pref = prefix st'
  when (column st' == 0 && usePrefix st' && not (null pref)) $ do
    modify $ \st -> st{ output = fromString pref : output st
                      , column = column st + realLength pref }
  modify $ \st -> st{ output = fromString s : output st
                    , column = column st + off
                    , newlines = 0 }
*/