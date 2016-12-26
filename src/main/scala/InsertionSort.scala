object InsertionSort extends Sort {
  override def sortBy[T]: ((T, T) => Int) => (Seq[T]) => Seq[T] = comparator => seqToSort => {
    def step(sortedSoFar : Seq[T], queue : Seq[T]) : Seq[T] = queue match {
      case Seq() => sortedSoFar
      case x::xs =>
        val (l, r) = sortedSoFar.span((comparator(_, x)).andThen(_ > 0))
        step(l ++ (x +: r), xs)
    }
    step(Seq(), seqToSort).reverse
  }
}
