package sort

trait Sort {
  def sortBy[T](comparator:((T,T) => Int))(seq : Seq[T]) : Seq[T]

  def sort[T](seq : Seq[T])(implicit ev$1: T => Ordered[T]) : Seq[T] =
    sortBy[T](_ compare _)(seq)

  def sortOn[T, K](f : (T => K))(implicit ev$1: K => Ordered[K]): Seq[T] => Seq[T] = {
    val comparator = (x : T, y : T) => f(x) compare f(y)
    sortBy[T](comparator)
  }
}
