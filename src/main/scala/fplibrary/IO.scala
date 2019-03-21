package fplibrary

final case class IO[+A](unsafeRun: () => A) extends AnyVal

object IO {
  def create[A](a: => A): IO[A] =
    IO(() => a)

  implicit val M: Monad[IO] = new Monad[IO] {
    final override def pure[A](a: => A): IO[A] =
      create(a)

    final override def map[A, B](ca: IO[A])(ab: A => B): IO[B] =
      IO.create {
        val a = ca.unsafeRun()
        val b = ab(a)

        b
      }

    final override def flatMap[A, B](ca: IO[A])(acb: A => IO[B]): IO[B] =
      IO.create {
        val a = ca.unsafeRun()
        val cb = acb(a)
        val b = cb.unsafeRun()

        b
      }

    final override def flatten[A](cca: IO[IO[A]]): IO[A] =
      IO.create {
        val ca = cca.unsafeRun()
        val a = ca.unsafeRun()

        a
      }
  }
}
