package lectures.collections

/**
  * Постарайтесь не использовать мутабильные коллекции и var
  * Подробнее о сортировке можно подсмотреть здесь - https://en.wikipedia.org/wiki/Merge_sort
  *
  */
object MergeSortImpl extends App {

  def mergeSort(data: Seq[Int]): Seq[Int] = {
    data match {
      case seq if seq.size > 1 =>
        val parts = seq.splitAt(seq.size / 2)
        merge(mergeSort(parts._1), mergeSort(parts._2))
      case _ => data
    }
  }

  private def merge(left: Seq[Int], right: Seq[Int]): Seq[Int] = {
    (left, right) match {
      case (Seq(x, xs@_*), Seq(y, ys@_*)) => if (x > y) y +: merge(left, ys) else x +: merge(xs, right)
      case (_, Nil) => left
      case (Nil, _) => right
    }
  }

  println(mergeSort(List(1, 5, 2, 6, 9, 2, 3, 0, 7)))
  println(mergeSort(List(2, 1)))
  println(mergeSort(List(1)))
  println(mergeSort(Nil))

  println(mergeSort(Vector(2, 1)))
}
