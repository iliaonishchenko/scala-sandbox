object Main extends App {
  sealed trait MyList[+A]
  case object Nul extends MyList[Nothing]
  case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]

  object MyList {
    def sum(ints: MyList[Int]): Int = ints match {
      case Nul => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def product(doubles: MyList[Double]): Double = doubles match {
      case Nul => 1
      case Cons(x, xs) => x * product(xs)
    }

    def apply[A](as: A*): MyList[A] = {
      if (as.isEmpty) Nul
      else Cons(as.head, apply(as.tail:_*)) 
    }
  }

  val myList: MyList[Int] = MyList(1,2,3,4,5)

  //it should be 15
  val sum = MyList.sum(myList)
  println(s"sum is equals to 15? ${sum==15}")
}