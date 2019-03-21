package fplibrary

object PointFree {
  def compose[A, B, C](ab: A => B, bc: B => C): A => C =
    a => {
      val b = ab(a)
      val c = bc(b)

      c
    }

  def composeKleisli[A, B, C, D[_]: FlatMap](adb: A => D[B], bdc: B => D[C]): A => D[C] =
    a => {
      val db = adb(a)
      val dc = FlatMap[D].flatMap(db)(bdc)

      dc
    }
}
