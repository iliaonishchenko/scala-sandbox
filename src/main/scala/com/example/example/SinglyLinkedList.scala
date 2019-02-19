object Main extends App {
  import scala.annotation.tailrec

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

    def dropWhile[A](xs: MyList[A])(f: A => Boolean): MyList[A] = xs match {
      case Cons(h, t) if f(h) => MyList.dropWhile(t)(f)
      case _ => xs
    }

    def foldRight[A, B](as: MyList[A], z: B)(f: (A, B) => B): B = as match {
      case Nul => z
      case Cons(x, xs) => 
      val res = f(x, MyList.foldRight(xs, z)(f))
      println(res)
      res
    }

    def sum2(as: MyList[Int]): Int = MyList.foldRight(as, 0)(_ + _)

    def length[A](as: MyList[A]): Int = MyList.foldRight(as, 0)((_, b) => b + 1)

    @tailrec
    def foldLeft[A, B](as: MyList[A], z: B)(f: (A, B) => B): B = as match {
      case Nul => z
      case Cons(x, xs) => MyList.foldLeft(xs, f(x, z))(f)
    }

    def sum3(as: MyList[Int]): Int = MyList.foldLeft(as, 0)(_ + _)

    def reverse[A](as: MyList[A]): MyList[A] = MyList.foldLeft(as, Nul: MyList[A])((a, acc) => Cons(a, acc))

    def inc(as: MyList[Int]): MyList[Int] = as match {
      case Nul => Nul
      case Cons(x, xs) => Cons(x + 1, inc(xs))
    }

    def stringify(as: MyList[Double]): MyList[String] = as match {
      case Nul => Nul
      case Cons(x, xs) => Cons(x.toString, stringify(xs))
    }

    def map[A, B](as: MyList[A])(f: A => B): MyList[B] = as match {
      case Nul => Nul
      case Cons(x, xs) => Cons(f(x), map(xs)(f))
    }

    def inc2(as: MyList[Int]): MyList[Int] = map(as)(_ + 3)
  }

  val myList: MyList[Int] = MyList(1,2,3,4,5)
  val myDoubleList: MyList[Double] = MyList(1.0, 2.0, 3.0, 4.0, 5.0)

  println(MyList.inc2(myList))
}