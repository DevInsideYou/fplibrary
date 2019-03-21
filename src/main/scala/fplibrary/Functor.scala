package fplibrary

trait Functor[C[_]] {
  def map[A, B](ca: C[A])(ab: A => B): C[B]

  @inline final def fmap[A, B](ca: C[A])(ab: A => B): C[B] =
    map(ca)(ab)
}

object Functor extends Summoner[Functor]
