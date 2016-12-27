
import org.scalatest.FlatSpec
import sort._

class Test extends FlatSpec {
  it should "work" in {
    val sortingAlgs : Seq[Sort] = Seq(
      InsertionSort,
      MergeSort,
      QuickSort,
      SelectionSort,
      ShellSort
    )

    val arraysToSort = (1 to 1).map(_ => (1 to 10).map(_ => (Math.random() * 100).toInt))

    for(alg <- sortingAlgs) {
      println(sortingAlgs.getClass)
      for(ar <- arraysToSort) {
        println(alg.sort[Int](ar))
      }
    }
  }
}
