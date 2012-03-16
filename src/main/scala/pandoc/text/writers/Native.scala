package pandoc.text.writers

import java.io.PrintStream

import pandoc.text.Writer

abstract class Native(val outStream: PrintStream) extends Writer {
  def out = outStream
  
  def asString[T](arg: T)(implicit man: Manifest[T]) {
  }
  
  def writeList[T](arg: List[T])(implicit man: Manifest[T]) {
    
  }
}