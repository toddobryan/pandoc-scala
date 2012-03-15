package pandoc.text

import java.io.PrintStream

trait Writer {
  def out: PrintStream
  
  def asString[T](implicit man: Manifest[T]): String
  
  def write[T](arg: T)(implicit man: Manifest[T]) {
    out.print(asString[T])
  }
  
  def writeList[T](arg: List[T])(implicit man: Manifest[T])
}