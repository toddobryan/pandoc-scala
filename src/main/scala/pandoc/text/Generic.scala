package pandoc.text

import shapeless.{Case1Aux, Poly}
import shapeless.Poly._

object Generic {
  import shapeless.SybClass._
  
  type EverywhereDown[F <: Poly, T] = HomAux[EverywhereDownAux[F], T]
  
  class EverywhereDownAux[F <: Poly] extends Poly
  
  object EverywhereDownAux {
    implicit def default[F <: Poly, T](implicit data : DataT[EverywhereDownAux[F], T], f : HomAux[F, T]) =
      Case1Aux[EverywhereDownAux[F], T, T](t => data.gmapT(f(t)))
  }
  
  def bottomUp[T](f: PartialFunction[Any, Any], t: T): T = {
    
  }
  
  def topDown[F <: Poly](f: F) = everywhereDown(f)
}