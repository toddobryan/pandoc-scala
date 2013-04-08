package texmath

package object types {
  sealed abstract class SymbolType
  case object Ord extends SymbolType
  case object Op extends SymbolType
  case object Bin extends SymbolType
  case object Rel extends SymbolType
  case object Open extends SymbolType
  case object Close extends SymbolType
  case object Pun extends SymbolType
  case object Accent extends SymbolType

  sealed abstract class Alignment
  case object AlignLeft extends Alignment
  case object AlignCenter extends Alignment
  case object AlignRight extends Alignment
  case object AlignDefault extends Alignment

  type ArrayLine = Stream[Stream[Exp]]

  sealed abstract class Exp
  case class ENumber(str: String) extends Exp
  case class EGrouped(lst: Stream[Exp]) extends Exp
  case class EDelimited(open: String, close: String, lst: Stream[Exp]) extends Exp
  case class EIdentifier(str: String) extends Exp
  case class EMathOperator(str: String) extends Exp
  case class ESymbol(typ: SymbolType, str: String) extends Exp
  case class ESpace(str: String) extends Exp
  case class EBinary(str: String, left: Exp, right: Exp) extends Exp
  case class ESub(one: Exp, two: Exp) extends Exp
  case class ESuper(one: Exp, two: Exp) extends Exp
  case class ESubsup(one: Exp, two: Exp, three: Exp) extends Exp
  case class EOver(one: Exp, two: Exp) extends Exp
  case class EUnder(one: Exp, two: Exp) extends Exp
  case class EUnderover(one: Exp, two: Exp, three: Exp) extends Exp
  case class EUp(one: Exp, two: Exp) extends Exp
  case class EDown(one: Exp, two: Exp) extends Exp
  case class EDownup(one: Exp, two: Exp, three: Exp) extends Exp
  case class EUnary(str: String, exp: Exp) extends Exp
  case class EScaled(str: String, exp: Exp) extends Exp
  case class EStretchy(exp: Exp) extends Exp
  case class EArray(alignment: Stream[Alignment], content: Stream[ArrayLine]) extends Exp
  case class EText(typ: TextType, str: String)
  
  sealed abstract class DisplayType
  case object DisplayBlock extends DisplayType
  case object DisplayInline extends DisplayType
  
  sealed abstract class TextType
  case object TextNormal extends TextType
  case object TextBold extends TextType
  case object TextItalic extends TextType
  case object TextMonospace extends TextType
  case object TextSansSerif extends TextType
  case object TextDoubleStruck extends TextType
  case object TextScript extends TextType
  case object TextFraktur extends TextType
  case object TextBoldItalic extends TextType
  case object TextBoldSansSerif extends TextType
  case object TextBoldSansSerifItalic extends TextType
  case object TextBoldScript extends TextType
  case object TextBoldFraktur extends TextType
  case object TextSansSerifItalic extends TextType
}