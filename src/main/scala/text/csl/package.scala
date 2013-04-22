package text

package object csl {
  type Abbrev = (String, Stream[(String, Map[String, String])])
  
  type TermMap = ((String, String), (String, String)) //TODO: ((String, Form), (String, String))
  
  type MacroMap = (String, Stream[Element])
  
  type CSOption = (String, String)

}