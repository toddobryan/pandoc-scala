package text.pandoc


import definition._

object Generic {
  def gmapT[T](f: (Any) => Any, t: T): T = {
    val res = t match {
      case Pandoc(m, c) => Pandoc(gmapT(f, m), gmapT(f, c))
      case Meta(t, a, d) => Meta(gmapT(f, t), gmapT(f, a), gmapT(f, d))
      case ListAttributes(n, s, d) => ListAttributes(gmapT(f, n), gmapT(f, s), gmapT(f, d))
      case KeyValue(k, v) => KeyValue(gmapT(f, k), gmapT(f, v))
      case Attr(i, c, a) => Attr(gmapT(f, i), gmapT(f, c), gmapT(f, a))
      case TableCell(w) => TableCell(gmapT(f, w))
      case Format(w) => Format(gmapT(f, w))
      case DefnItem(i, d) => DefnItem(gmapT(f, i), gmapT(f, d))
      case Plain(c) => Plain(gmapT(f, c))
      case Para(c) => Para(gmapT(f, c))
      case CodeBlock(a, s) => CodeBlock(gmapT(f, a), gmapT(f, s))
      case RawBlock(ft, s) => RawBlock(gmapT(f, ft), gmapT(f, s))
      case BlockQuote(c) => BlockQuote(gmapT(f, c))
      case OrderedList(a, i) => OrderedList(gmapT(f, a), gmapT(f, i))
      case BulletList(i) => BulletList(gmapT(f, i))
      case DefinitionList(i) => DefinitionList(gmapT(f, i))
      case Header(l, c) => Header(gmapT(f, l), gmapT(f, c))
      case Table(c, a, w, h, r) => Table(gmapT(f, c), gmapT(f, a), gmapT(f, w), gmapT(f, h), gmapT(f, r))
      case Target(u, t) => Target(gmapT(f, u), gmapT(f, t))
      case Str(s) => Str(gmapT(f, s))
      case Emph(c) => Emph(gmapT(f, c))
      case Strong(c) => Strong(gmapT(f, c))
      case Strikeout(c) => Strikeout(gmapT(f, c))
      case Superscript(c) => Superscript(gmapT(f, c))
      case Subscript(c) => Subscript(gmapT(f, c))
      case SmallCaps(c) => SmallCaps(gmapT(f, c))
      case Quoted(k, c) => Quoted(gmapT(f, k), gmapT(f, c))
      case Cite(ci, co) => Cite(gmapT(f, ci), gmapT(f, co))
      case Code(a, s) => Code(gmapT(f, a), gmapT(f, s))
      case Math(k, s) => Math(gmapT(f, k), gmapT(f, s))
      case RawInline(ft, s) => RawInline(gmapT(f, ft), gmapT(f, s))
      case Link(l, t) => Link(gmapT(f, l), gmapT(f, t))
      case Image(a, t) => Image(gmapT(f, a), gmapT(f, t))
      case Note(c) => Note(gmapT(f, c))
      case Citation(i, p, s, m, n, h) => 
          Citation(gmapT(f, i), gmapT(f, p), gmapT(f, s), gmapT(f, m), gmapT(f, n), gmapT(f, h))
      
      case x :: xs => gmapT(f, x) :: gmapT(f, xs)
      case x => f(x)
    }
    res.asInstanceOf[T]
  }
  
  def gmapQ[T, U](f: PartialFunction[Any, U], t: T): List[U] = {
    t match {
      case Pandoc(m, c) => gmapQ(f, m) ++ gmapQ(f, c)
      case Meta(t, a, d) => gmapQ(f, t) ++ gmapQ(f, a) ++ gmapQ(f, d)
      case ListAttributes(n, s, d) => gmapQ(f, n) ++ gmapQ(f, s) ++ gmapQ(f, d)
      case KeyValue(k, v) => gmapQ(f, k) ++ gmapQ(f, v)
      case Attr(i, c, a) => gmapQ(f, i) ++ gmapQ(f, c) ++ gmapQ(f, a)
      case TableCell(w) => gmapQ(f, w)
      case Format(w) => gmapQ(f, w)
      case DefnItem(i, d) => gmapQ(f, i) ++ gmapQ(f, d)
      case Plain(c) => gmapQ(f, c)
      case Para(c) => gmapQ(f, c)
      case CodeBlock(a, s) => gmapQ(f, a) ++ gmapQ(f, s)
      case RawBlock(ft, s) => gmapQ(f, ft) ++ gmapQ(f, s)
      case BlockQuote(c) => gmapQ(f, c)
      case OrderedList(a, i) => gmapQ(f, a) ++ gmapQ(f, i)
      case BulletList(i) => gmapQ(f, i)
      case DefinitionList(i) => gmapQ(f, i)
      case Header(l, c) => gmapQ(f, l) ++ gmapQ(f, c)
      case Table(c, a, w, h, r) => gmapQ(f, c) ++ gmapQ(f, a) ++ gmapQ(f, w) ++ gmapQ(f, h) ++ gmapQ(f, r)
      case Target(u, t) => gmapQ(f, u) ++ gmapQ(f, t)
      case Str(s) => gmapQ(f, s)
      case Emph(c) => gmapQ(f, c)
      case Strong(c) => gmapQ(f, c)
      case Strikeout(c) => gmapQ(f, c)
      case Superscript(c) => gmapQ(f, c)
      case Subscript(c) => gmapQ(f, c)
      case SmallCaps(c) => gmapQ(f, c)
      case Quoted(k, c) => gmapQ(f, k) ++ gmapQ(f, c)
      case Cite(ci, co) => gmapQ(f, ci) ++ gmapQ(f, co)
      case Code(a, s) => gmapQ(f, a) ++ gmapQ(f, s)
      case Math(k, s) => gmapQ(f, k) ++ gmapQ(f, s)
      case RawInline(ft, s) => gmapQ(f, ft) ++ gmapQ(f, s)
      case Link(l, t) => gmapQ(f, l) ++ gmapQ(f, t)
      case Image(a, t) => gmapQ(f, a) ++ gmapQ(f, t)
      case Note(c) => gmapQ(f, c)
      case Citation(i, p, s, m, n, h) => 
          gmapQ(f, i) ++ gmapQ(f, p) ++ gmapQ(f, s) ++ gmapQ(f, m) ++ gmapQ(f, n) ++ gmapQ(f, h)
      
      case x :: xs => gmapQ(f, x) ++ gmapQ(f, xs)
      case x if (f.isDefinedAt(x)) => List(f(x))
      case _ => Nil
    }
  }


  def everywhereUp[T](f: (Any) => Any)(t: T): T = {
    f(gmapT(everywhereUp(f), t)).asInstanceOf[T]
  }
  
  def everywhereDown[T](f: (Any) => Any)(t: T): T = {
    gmapT(everywhereDown(f), f(t).asInstanceOf[T])
  }
  
  def everything[T, R](k: (R, R) => R, f: PartialFunction[Any, R])(t: T): R = {
    gmapQ[T, R]((everything(k, f) _).asInstanceOf[PartialFunction[Any, R]], t).foldLeft(f(t))(k)
  }
  
  def bottomUp[T](f: (Any) => Any, t: T): T = everywhereUp(f)(t)
  
  def topDown[T](f: (Any) => Any, t: T): T = everywhereDown(f)(t)
  
  def queryWith[T, R](f: PartialFunction[Any, R], t: T) = gmapQ(f, t) // I'm not sure this is right...
  
}