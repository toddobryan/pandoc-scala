package text.csl

case class Style(
  version: String,
  klass: String,
  info: Option[String], //TODO: should be Option[CSInfo]
  defaultLocale: String,
  locale: Stream[Locale],
  abbrevs: Stream[Abbrev],
  options: Stream[CSOption],
  macros: Stream[(String, Stream[Element])],
  citation: Citation,
  biblio: Option[Bibliography]
)

case class Locale(
  version: String,
  lang: String,
  options: Stream[CSOption],
  termMap: Stream[TermMap],
  date: Stream[Element]
)

case class Citation(
  options: Stream[CSOption],
  sort: Stream[String],  //TODO: should be Stream[Sort]
  layout: Layout
)

case class Bibliography(
  options: Stream[CSOption],
  sort: Stream[String], //TODO: should be Stream[Sort]
  layout: Layout
)

case class Layout(
  format: String, //TODO: should be Formatting
  delim: String, //TODO: should be Delimiter
  elements: Stream[Element]
)

sealed abstract class Element




