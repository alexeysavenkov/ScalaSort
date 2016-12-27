package sort

import scala.annotation.tailrec

object MergeSort extends Sort {
  override def sortBy[T](comparator:((T,T) => Int))(seqToSort : Seq[T]) : Seq[T] = {
    @tailrec
    def merge(xs : Seq[T], ys : Seq[T], accum : Seq[T] = Seq()) : Seq[T] = (xs, ys) match {
      case (Seq(), _) => ys.reverse ++ accum
      case (_, Seq()) => xs.reverse ++ accum
      case (x::rx, y::ry) =>
        if(comparator(x, y) >= 0)
          merge(rx, ys, x +: accum)
        else
          merge(xs, ry, y +: accum)
    }

    @tailrec
    def step(seq : Seq[Seq[T]]) : Seq[T] = seq match {
      case Seq(xs) => xs
      case xss =>
        val afterStep = xss.grouped(2).map({
          case Seq(xs) => xs
          case Seq(xs, ys) => merge(xs.reverse, ys.reverse)
        }).toSeq
        step(afterStep)
    }

    step(seqToSort.map(Seq(_)))
  }
}
