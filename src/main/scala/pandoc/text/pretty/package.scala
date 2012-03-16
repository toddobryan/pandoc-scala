package pandoc.text

package object pretty {
  case class RenderState(
      output: List[String], // in reverse order
      prefix: String,
      usePrefix: Boolean,
      lineLength: Option[Int],
      column: Int,
      newLines: Int)
  
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
  def empty = Doc(Nil)
  
  def cat(docs: List[Doc]): Doc = Doc(docs.flatMap(_.content))
  def hcat = cat _
  def hsep(docs: List[Doc]): Doc = docs.foldRight[Doc](empty)((l: Doc, r: Doc) => l <+> r)
  def vcat(docs: List[Doc]): Doc = docs.foldRight[Doc](empty)((l: Doc, r: Doc) => l %% r)
  def vsep(docs: List[Doc]): Doc = docs.foldRight[Doc](empty)((l: Doc, r: Doc) => l %+% r)
  
  def outp(offset: Int, s: String, state: RenderState): RenderState = {
    if (offset <= 0) {
      val rawPref = state.prefix
      if (state.column == 0 && state.usePrefix && rawPref != "") {
        val pref: List[Char] = rawPref.toCharArray.toList.reverse.dropWhile(_.isSpaceChar).reverse
        RenderState(pref.mkString :: state.output, state.prefix, state.usePrefix, 
            state.lineLength, state.column + realLength(pref), state.newLines)
      } else if (offset < 0) {
        RenderState(s :: state.output, state.prefix, state.usePrefix, 
            state.lineLength, 0, state.newLines + 1)
      } else {
        state
      }
    } else {
      val pref = state.prefix
      val statePrime: RenderState = 
        if (state.column == 0 && state.usePrefix && pref != "") {
    	  RenderState(pref :: state.output, state.prefix, state.usePrefix,
    	      state.lineLength, state.column + realLength(pref.toCharArray.toList), state.newLines)
        } else {
          state
        }
      RenderState(s :: statePrime.output, statePrime.prefix, statePrime.usePrefix,
              statePrime.lineLength, statePrime.column + offset, 0)
    }
  }
  
  def render(lineLength: Option[Int], doc: Doc): String = {
    renderDoc(doc, RenderState(Nil, "", true, lineLength, 0, 2)).output.reverse.mkString
  }
  
  def renderDoc(doc: Doc, state: RenderState): RenderState = {
    renderList(doc.content, state)
  }
  
  def renderList(elts: List[D], state: RenderState): RenderState = {
    elts match {
      case Nil => state
      case Text(offset, s) :: xs => outp(offset, s, state)
    }
  }

/*
-- | Renders a 'Doc'.  @render (Just n)@ will use
-- a line length of @n@ to reflow text on breakable spaces.
-- @render Nothing@ will not reflow text.
render :: (Monoid a, IsString a)
       => Maybe Int -> Doc -> a
render linelen doc = fromString . mconcat . reverse . output $
  execState (renderDoc doc) startingState
   where startingState = RenderState{
                            output = mempty
                          , prefix = ""
                          , usePrefix = True
                          , lineLength = linelen
                          , column = 0
                          , newlines = 2 }

renderDoc :: (IsString a, Monoid a)
          => Doc -> DocState a
renderDoc = renderList . toList . unDoc

renderList :: (IsString a, Monoid a)
           => [D] -> DocState a
renderList [] = return ()
renderList (Text off s : xs) = do
  outp off s
  renderList xs

renderList (Prefixed pref d : xs) = do
  st <- get
  let oldPref = prefix st
  put st{ prefix = prefix st ++ pref }
  renderDoc d
  modify $ \s -> s{ prefix = oldPref }
  renderList xs

renderList (Flush d : xs) = do
  st <- get
  let oldUsePrefix = usePrefix st
  put st{ usePrefix = False }
  renderDoc d
  modify $ \s -> s{ usePrefix = oldUsePrefix }
  renderList xs

renderList (BeforeNonBlank d : xs) =
  case xs of
    (x:_) | isBlank x -> renderList xs
          | otherwise -> renderDoc d >> renderList xs
    []                -> renderList xs

renderList (BlankLine : xs) = do
  st <- get
  case output st of
     _ | newlines st > 1 || null xs -> return ()
     _ | column st == 0 -> do
       outp (-1) "\n"
     _         -> do
       outp (-1) "\n"
       outp (-1) "\n"
  renderList xs

renderList (CarriageReturn : xs) = do
  st <- get
  if newlines st > 0 || null xs
     then renderList xs
     else do
       outp (-1) "\n"
       renderList xs

renderList (NewLine : xs) = do
  outp (-1) "\n"
  renderList xs

renderList (BreakingSpace : CarriageReturn : xs) = renderList (CarriageReturn:xs)
renderList (BreakingSpace : NewLine : xs) = renderList (NewLine:xs)
renderList (BreakingSpace : BlankLine : xs) = renderList (BlankLine:xs)
renderList (BreakingSpace : BreakingSpace : xs) = renderList (BreakingSpace:xs)
renderList (BreakingSpace : xs) = do
  let isText (Text _ _)       = True
      isText (Block _ _)      = True
      isText _                = False
  let isBreakingSpace BreakingSpace = True
      isBreakingSpace _             = False
  let xs' = dropWhile isBreakingSpace xs
  let next = takeWhile isText xs'
  st <- get
  let off = sum $ map offsetOf next
  case lineLength st of
        Just l | column st + 1 + off > l -> do
          outp (-1) "\n"
          renderList xs'
        _  -> do
          outp 1 " "
          renderList xs'

renderList (b1@Block{} : b2@Block{} : xs) =
  renderList (mergeBlocks False b1 b2 : xs)

renderList (b1@Block{} : BreakingSpace : b2@Block{} : xs) =
  renderList (mergeBlocks True b1 b2 : xs)

renderList (Block width lns : xs) = do
  st <- get
  let oldPref = prefix st
  case column st - realLength oldPref of
        n | n > 0 -> modify $ \s -> s{ prefix = oldPref ++ replicate n ' ' }
        _         -> return ()
  renderDoc $ blockToDoc width lns
  modify $ \s -> s{ prefix = oldPref }
  renderList xs

mergeBlocks :: Bool -> D -> D -> D
mergeBlocks addSpace (Block w1 lns1) (Block w2 lns2) =
  Block (w1 + w2 + if addSpace then 1 else 0) $
     zipWith (\l1 l2 -> pad w1 l1 ++ l2) (lns1 ++ empties) (map sp lns2 ++ empties)
    where empties = replicate (abs $ length lns1 - length lns2) ""
          pad n s = s ++ replicate (n - realLength s) ' '
          sp "" = ""
          sp xs = if addSpace then (' ' : xs) else xs
mergeBlocks _ _ _ = error "mergeBlocks tried on non-Block!"

blockToDoc :: Int -> [String] -> Doc
blockToDoc _ lns = text $ intercalate "\n" lns

offsetOf :: D -> Int
offsetOf (Text o _)       = o
offsetOf (Block w _)      = w
offsetOf BreakingSpace    = 1
offsetOf _                = 0
*/
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
  def space: Doc = Doc(List(BreakingSpace))
  def cr: Doc = Doc(List(CarriageReturn))
  def blankline: Doc = Doc(List(BlankLine))
  def prefixed(prefix: String, doc: Doc): Doc = Doc(List(Prefixed(prefix, doc)))
  def flush(doc: Doc): Doc = Doc(List(Flush(doc)))
  def nest(indent: Int, doc: Doc): Doc = prefixed(" " * indent, doc)
  def hang(indent: Int, start: Doc, doc: Doc): Doc = start <> nest(indent, doc)
  def beforeNonBlank(doc: Doc): Doc = Doc(List(BeforeNonBlank(doc)))
  def noWrap(doc: Doc): Doc = {
    val unwrapped = doc.content map {(d: D) =>
      d match {
        case BreakingSpace => Text(1, " ")
        case x => x
      }
    }
    Doc(unwrapped)
  }
  def offset(doc: Doc): Int = render(None, doc).split('\n').toList.
	  map((s: String) => realLength(s.toCharArray.toList)).foldLeft(0)((l: Int, r: Int) => math.max(l, r))
  def block(filler: (String => String), width: Int)(doc: Doc): Doc = {
    Doc(List(Block(width, chop(width, render(Some(width), doc)).map(filler(_)))))
  }
  def lblock(width: Int, doc: Doc): Doc = block((s: String) => s, width)(doc)
  def rblock(width: Int, doc: Doc): Doc = {
    block((s: String) => (" " * (width - realLength(s.toCharArray.toList))) + s, width)(doc)
  }
  def cblock(width: Int, doc: Doc): Doc = {
    block((s: String) => (" " * ((width - realLength(s.toCharArray.toList)) / 2)) + s, width)(doc)
  }
  def height(doc: Doc): Int = render(None, doc).split('\n').length
  def chop(width: Int, s: String): List[String] = {
    if (s == "") Nil
    else s.toCharArray.toList.span(_ != '\n') match {
      case (xs, ys) if (realLength(xs) <= width) => ys match {
        case Nil => List(xs.mkString)
        case _ :: Nil => List(xs.mkString, "")
        case _ :: zs => xs.mkString :: chop(width, zs.mkString)
      }
    }
  }
    
  def inside(start: Doc, end: Doc)(contents: Doc): Doc = start <> contents <> end
  def braces(doc: Doc): Doc = inside(char('{'), char('}'))(doc)
  def brackets(doc: Doc): Doc = inside(char('['), char(']'))(doc)
  def parens(doc: Doc): Doc = inside(char('('), char(')'))(doc)
  def quotes(doc: Doc): Doc = inside(char('\''), char('\''))(doc)
  def doubleQuotes(doc: Doc): Doc = inside(char('"'), char('"'))(doc)
  
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