package lectures.collections

/**
  * Постарайтесь не использовать мутабильные коллекции и var
  * Подробнее о сортировке можно подсмотреть здесь - https://en.wikipedia.org/wiki/Merge_sort
  *
  */
object MergeSortImpl extends App {

  def mergeSort(data: Seq[Int]): Seq[Int] = {
    data match {
      case Nil => data
      case List(_) => data
      case list => merge(mergeSort(list.take(list.length / 2)), mergeSort(list.drop(list.length / 2)), Nil)
    }
  }

  private def merge(list1: Seq[Int], list2: Seq[Int], acc: Seq[Int]): Seq[Int] = {
    (list1, list2) match {
      case (h1 :: t1, h2 :: t2) => if (h1 > h2) merge(list1, t2, acc :+ h2) else merge(t1, list2, acc :+ h1)
      case (_, Nil) => acc ++ list1
      case (Nil, _) => acc ++ list2
    }
  }

  println(mergeSort(List(1, 5, 2, 6, 9, 2, 3, 0, 7)))
  println(mergeSort(List(2, 1)))
  println(mergeSort(List(1)))
  println(mergeSort(Nil))
}
