package fplibrary

trait Monad[C[_]] extends FlatMap[C] with Applicative[C] {
  def flatten[A](cca: C[C[A]]): C[A]

  @inline final def join[A](cca: C[C[A]]): C[A] =
    flatten(cca)
}

object Monad extends Summoner[Monad]
