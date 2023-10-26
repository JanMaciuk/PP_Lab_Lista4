import scala.annotation.tailrec

object Main {
  def main(args: Array[String]): Unit = {
    Log("WARN")("2023-10-26 : 16:00")("Nuclear coolant leak detected in SSD, check engine")
    println(Map(List(1,2,3,4,5), (x: Int) => x*x))
    println(Filter(List(1,2,3,4,5), (x: Int) => x%2 == 0))
    println(Reduce(List(1,2,3,4,5), (x: Int,y:Int) => x+y, 0))
    println(Average(List(1,2,3,4,5)))
    println(Acronym(List("Zdefraudujemy","Ubezpieczonemu","Skladki")))
    println(SquaresSmallerThanSum(List(1,2,3,4,5)))
  }

  private def Log(prefix: String)(datetime: String)(text: String): Unit = {
    println("[" + prefix + "] " + datetime + "\t" + text)
  }

  private def Map[A](list:List[A], func: A => A): List[A] = {
    list match {
      case Nil => Nil
      case head :: tail => func(head) :: Map(tail, func)
    }
  }

  private def Filter[A](list:List[A], func: A =>Boolean ): List[A] = {
    list match {
      case Nil => Nil
      case head :: tail => if (func(head)) head :: Filter(tail, func) else Filter(tail, func)
    }
  }

  @tailrec
  private def Reduce[A](list: List[A], func: (A,Int) => Int, acc: Int): Int = {
    list match {
      case Nil => acc
      case head :: tail => Reduce(tail, func, func(head, acc))
    }
  }
  private def Average(list: List[Int]): Int = {
    val sum = list.reduce(_ + _)
    sum / list.length
  }

  private def Acronym(list: List[String]): String = {
    val charlist = list.map(_.charAt(0))
    new String(charlist.toArray)
  }

  private def SquaresSmallerThanSum(list: List[Int]): List[Int] = {
    val sum = list.sum
    list.filter(isSquareSmaller(_, sum))
  }

  private def isSquareSmaller(x: Int, y: Int): Boolean = {
    x*x < y
  }
}