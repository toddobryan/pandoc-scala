package text.highlighting.kate

import java.awt.Color

object Types {
  sealed abstract class TokenType
  case object KeywordTok extends TokenType
  case object DataTypeTok extends TokenType
  case object DecValTok extends TokenType
  case object BaseNTok extends TokenType
  case object FloatTok extends TokenType
  case object CharTok extends TokenType
  case object StringTok extends TokenType
  case object CommentTok extends TokenType
  case object OtherTok extends TokenType
  case object AlertTok extends TokenType
  case object FunctionTok extends TokenType
  case object RegionMarkerTok extends TokenType
  case object ErrorTok extends TokenType
  case object NormalTok extends TokenType

  case class TokenStyle(
      color: Option[Color] = None,
      background: Option[Color] = None,
      bold: Boolean = false,
      italic: Boolean = false,
      underline: Boolean = false)

  case class Style(
      tokenStyles: Map[TokenType, TokenStyle], 
      defaultColor: Option[Color],
      backgroundColor: Option[Color],
      lineNumberColor: Option[Color],
      lineNumberBackgroundColor: Option[Color])

  object Pygments extends Style(
      Map(KeywordTok -> TokenStyle(color = Some(new Color(0x0, 0x70, 0x20)), 
               					   bold = true),
    	  DataTypeTok -> TokenStyle(color = Some(new Color(0x90, 0x20, 0x00))),
    	  DecValTok -> TokenStyle(color = Some(new Color(0x40, 0xa0, 0x70))),
    	  BaseNTok -> TokenStyle(color = Some(new Color(0x40, 0xa0, 0x70))),
    	  FloatTok -> TokenStyle(color = Some(new Color(0x40, 0xa0, 0x70))),
    	  CharTok -> TokenStyle(color = Some(new Color(0x40, 0xa0, 0x70))),
    	  StringTok -> TokenStyle(color = Some(new Color(0x40, 0xa0, 0x70))),
    	  CommentTok -> TokenStyle(color = Some(new Color(0x60, 0xa0, 0xb0)), italic = true),
    	  OtherTok -> TokenStyle(color = Some(new Color(0x00, 0x70, 0x20))),
    	  AlertTok -> TokenStyle(color = Some(new Color(0xff, 0x00, 0x00)), bold = true),
    	  FunctionTok -> TokenStyle(color = Some(new Color(0x06, 0x28, 0x7e))),
    	  ErrorTok -> TokenStyle(color = Some(new Color(0xff, 0x00, 0x00)))
      ),
      None, None, Some(new Color(170, 170, 170)), None)
  
}