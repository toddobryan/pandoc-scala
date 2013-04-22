package text.pandoc

object Xml {
  def stripTags(s: String): String = stripTags(s.toList).mkString
  
  def stripTags(cs: List[Char]): List[Char] = cs match {
    case '<' :: xs => {
      val (_, rest) = xs.span(_ != '>')
      if (rest.isEmpty) Nil else stripTags(rest.tail)
    }
    case x :: xs => x :: stripTags(xs)
    case Nil => Nil
  }
}