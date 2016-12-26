trait Sort {
  def sortBy[T] : ((T, T) => Int) => Seq[T] => Seq[T]

  def sort[T <: Ordered[T]] : Seq[T] => Seq[T] =
    sortBy[T](_ compare _)

  def sortOn[T, K <: Ordered[K]](f : ((T, T) => K)) : Seq[T] => Seq[T] =
    sortBy[T]((x, y) => f(x) compare f(y))
}
