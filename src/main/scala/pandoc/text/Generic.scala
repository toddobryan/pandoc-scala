package pandoc.text


import Definition._
import shapeless.Poly
import shapeless.SybClass._

object Generic {
  def bottomUp[T](f: Poly): EverywhereAux[f.type] = {
    everywhere(f)
  }
}