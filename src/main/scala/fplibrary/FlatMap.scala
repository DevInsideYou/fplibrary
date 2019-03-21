package fplibrary

trait FlatMap[C[_]] extends Functor[C] {
  def flatMap[A, B](ca: C[A])(acb: A => C[B]): C[B]

  @inline final def bind[A, B](ca: C[A])(acb: A => C[B]): C[B] =
    flatMap(ca)(acb)

  @inline final def >>=[A, B](ca: C[A])(acb: A => C[B]): C[B] =
    flatMap(ca)(acb)
}

object FlatMap extends Summoner[FlatMap]
