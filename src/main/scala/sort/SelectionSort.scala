package sort

import scala.annotation.tailrec

object SelectionSort extends Sort {
  override def sortBy[T](comparator:((T,T) => Int))(seqToSort : Seq[T]) : Seq[T] = {
    @tailrec
    def maximum(seq : Seq[T], maxSoFar : Option[T] = None) : T =
      seq match {
        case Seq() => maxSoFar.get
        case x +: xs => maximum(xs, if(maxSoFar.isEmpty || comparator(x, maxSoFar.get) > 0) Some(x) else maxSoFar)
      }
    def popMaximum(seq : Seq[T]) : (T, Seq[T]) = {
      val max = maximum(seq)
      val (l, r) = seq.span(_ != max)
      (r.head, l ++ r.tail)
    }
    @tailrec
    def step(sortedSoFar : Seq[T], queue : Seq[T]) : Seq[T] =
      queue match {
        case Seq() => sortedSoFar
        case xs =>
          val (max, rest) = popMaximum(xs)
          step(max +: sortedSoFar, rest)
      }

    step(Seq(), seqToSort)
  }
}
