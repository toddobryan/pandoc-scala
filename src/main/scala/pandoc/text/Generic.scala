package pandoc.text

import shapeless.{Case1Aux, Poly}
import shapeless.Poly._

object Generic {
  import shapeless.SybClass._
  
  class EverywherePrimeAux[F <: Poly] extends Poly
  
  object EverywherePrimeAux {
    implicit def default[F <: Poly, T](implicit data : DataT[EverywhereAux[F], T], f : HomAux[F, T]) =
      Case1Aux[EverywhereAux[F], T, T](t => data.gmapT(f(t)))
  }
  
  def everywherePrime[F <: Poly](f: F) = new EverywhereAux[f.type]

}