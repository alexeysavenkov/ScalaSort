package sort

import scalaz._, Scalaz._, scalaz.Maybe._
import scala.annotation.tailrec

object ShellSort extends Sort {
  override def sortBy[T](comparator:((T,T) => Int))(seqToSort : Seq[T]) : Seq[T] = {
    def gaps(gapSize : Int, seq : Seq[T]) : Seq[Seq[T]] =
      (0 until gapSize).map(offset => seq.drop(offset).grouped(gapSize).map(_.head).toSeq)

    @tailrec
    def ungaps(deq : Dequeue[Seq[T]], accum : Seq[T] = Seq()) : Seq[T] =
      deq.uncons match {
        case Empty() => accum.reverse
        case Just((Seq(), rest)) => ungaps(rest, accum)
        case Just((Seq(x), rest)) => ungaps(rest, x +: accum)
        case Just((x +: xs, rest)) => ungaps(rest :+ xs, x +: accum)
      }

    @tailrec
    def sortRec(gapSize : Int, seq : Seq[T]) : Seq[T] = {
      if(gapSize < 1) {
        seq
      } else {
        val gapSeq = gaps(gapSize, seq)
        val sortedGaps = gapSeq.map(InsertionSort.sortBy(comparator))
        val afterStep = ungaps(Dequeue(sortedGaps: _*))
        sortRec(gapSize/2, afterStep)
      }
    }

    sortRec(seqToSort.length / 3, seqToSort)
  }
}
