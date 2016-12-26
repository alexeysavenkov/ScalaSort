object QuickSort extends Sort {
  override def sortBy[T] : ((T, T) => Int) => Seq[T] => Seq[T] = comparator => {
    case xs@(Seq() | Seq(_)) => xs
    case xs =>
      val pivot = xs.head
      val (l, r) = xs.partition((comparator(_, pivot)).andThen(_ < 0))
      l ++ r
  }
}
