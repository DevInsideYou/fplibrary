package fplibrary

final case class State[S, +A](run: S => (A, S)) extends AnyVal {
  def runA(s: S): A = run(s)._1
  def runS(s: S): S = run(s)._2
}

object State {
  def create[S, A](a: => A): State[S, A] =
    State(s => (a, s))

  implicit def M[S]: Monad[State[S, ?]] = new Monad[State[S, ?]] {
    final override def pure[A](a: => A): State[S, A] =
      create(a)

    final override def map[A, B](ca: State[S, A])(ab: A => B): State[S, B] =
      State { s =>
        val (a, s1) = ca.run(s)
        val b = ab(a)

        b -> s1
      }

    final override def flatMap[A, B](ca: State[S, A])(acb: A => State[S, B]): State[S, B] =
      State { s =>
        val (a, s1) = ca.run(s)
        val cb = acb(a)
        val (b, s2) = cb.run(s1)

        b -> s2
      }

    final override def flatten[A](cca: State[S, State[S, A]]): State[S, A] =
      State { s =>
        val (ca, s1) = cca.run(s)
        val (a, s2) = ca.run(s1)

        a -> s2
      }
  }
}
