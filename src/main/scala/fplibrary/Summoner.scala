package fplibrary

trait Summoner[D[_[_]]] {
  def apply[C[_]: D]: D[C] =
    implicitly[D[C]]
}
