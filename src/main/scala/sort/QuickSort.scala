package sort

object QuickSort extends Sort {
  override def sortBy[T](comparator:((T,T) => Int))(seq : Seq[T]) : Seq[T] = {
    def partition(xs : Seq[T], pivotIndex : Int = 0) : (Seq[T], Seq[T]) = {
      val pivot = xs(pivotIndex)
      val (l, r) = xs.partition((comparator(_: T, pivot)).andThen(_ <= 0))
      if(l.isEmpty || r.isEmpty)
        partition(xs, pivotIndex + 1)
      else
        (l, r)
    }
    seq match {
      case xs@(Seq() | Seq(_)) => xs
      case Seq(x, y) =>
        if(comparator(x, y) > 0)
          Seq(y, x)
        else
          Seq(x, y)
      case xs =>
        val (l, r) = partition(xs)
        sortBy(comparator)(l) ++ sortBy(comparator)(r)
    }
  }
}
