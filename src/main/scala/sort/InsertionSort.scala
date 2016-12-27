package sort

import scala.annotation.tailrec

object InsertionSort extends Sort {
  override def sortBy[T](comparator:((T,T) => Int))(seqToSort : Seq[T]) : Seq[T] = {
    @tailrec
    def step(sortedSoFar : Seq[T], queue : Seq[T]) : Seq[T] = queue match {
      case Seq() => sortedSoFar
      case x +: xs =>
        val (l, r) = sortedSoFar.span((comparator(_ : T, x)).andThen(_ > 0))
        step(l ++ (x +: r), xs)
    }
    step(Seq(), seqToSort).reverse
  }
}
