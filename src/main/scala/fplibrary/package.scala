package object fplibrary {
  implicit final class InfixNotationForPointFree[A, B](private val ab: A => B) extends AnyVal {
    @inline def -->[C](bc: B => C): A => C =
      PointFree.compose(ab, bc)
  }

  implicit final class InfixNotationForPointFreeKleisli[A, B, D[_]](private val adb: A => D[B]) extends AnyVal {
    @inline def >=>[C](bdc: B => D[C])(implicit M: Monad[D]): A => D[C] =
      PointFree.composeKleisli(adb, bdc)
  }

  implicit final class InfixNotationForHigherKinds[C[_], A](private val ca: C[A]) extends AnyVal {
    @inline def map[B](ab: A => B)(implicit F: Functor[C]): C[B] =
      F.map(ca)(ab)

    @inline def fmap[B](ab: A => B)(implicit F: Functor[C]): C[B] =
      F.map(ca)(ab)

    @inline def flatMap[B](acb: A => C[B])(implicit M: FlatMap[C]): C[B] =
      M.flatMap(ca)(acb)

    @inline def bind[B](acb: A => C[B])(implicit M: FlatMap[C]): C[B] =
      M.flatMap(ca)(acb)

    @inline def >>=[B](acb: A => C[B])(implicit M: FlatMap[C]): C[B] =
      M.flatMap(ca)(acb)

    @inline def flatten[D](implicit M: Monad[C], view: C[A] => C[C[D]]): C[D] =
      M.flatten(view(ca))

    @inline def join[D](implicit M: Monad[C], view: C[A] => C[C[D]]): C[D] =
      M.flatten(view(ca))
  }
}
