package fplibrary

final case class Reader[-D, +A](run: D => A) extends AnyVal

object Reader {
  def create[D, A](a: => A): Reader[D, A] =
    Reader(d => a)

  implicit def M[D]: Monad[Reader[D, ?]] = new Monad[Reader[D, ?]] {
    final override def pure[A](a: => A): Reader[D, A] =
      create(a)

    final override def map[A, B](ca: Reader[D, A])(ab: A => B): Reader[D, B] =
      Reader { d =>
        val a = ca.run(d)
        val b = ab(a)

        b
      }

    final override def flatMap[A, B](ca: Reader[D, A])(acb: A => Reader[D, B]): Reader[D, B] =
      Reader { d =>
        val a = ca.run(d)
        val cb = acb(a)
        val b = cb.run(d)

        b
      }

    final override def flatten[A](cca: Reader[D, Reader[D, A]]): Reader[D, A] =
      Reader { d =>
        val ca = cca.run(d)
        val a = ca.run(d)

        a
      }
  }
}
