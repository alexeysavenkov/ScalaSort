object MergeSort extends Sort {
  override def sortBy[T]: ((T, T) => Int) => Seq[T] => Seq[T] = comparator => seqToSort => {
    def merge(xs : Seq[T], ys : Seq[T], accum : Seq[T] = Seq()) : Seq[T] = (xs, ys) match {
      case (Seq(), _) => ys ++ accum
      case (_, Seq()) => xs ++ accum
      case (x::rx, y::ry) =>
        if(comparator(x, y) < 0)
          merge(xs, ry, y +: accum)
        else
          merge(rx, ys, x +: accum)
    }
    def step : Seq[Seq[T]] => Seq[T] = {
      case Seq(xs) => xs
      case xss =>
        step(xss.grouped(2).map({
          case Seq(xs) => xs
          case Seq(xs, ys) => merge(xs, ys)
        }).toSeq)
    }
    step(seqToSort.map(Seq(_)))
  }
}
