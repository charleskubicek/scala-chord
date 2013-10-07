package ck.chord

object Collections{
  implicit class IterableExtensions[A](it: Iterable[A]) {
    def slidingPair: Iterator[Pair[A, A]] = {
      it.sliding(2) map {
        e => Pair(e.head, e.last)
      }
    }
  }
}
