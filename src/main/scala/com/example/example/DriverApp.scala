package com.example.example

object DriverApp extends App {
  println("Hello world")
}

object MergeSort {
  def mergeSort(list: List[Int]): List[Int] = {
    if (list.length == 1) list
    else {
      val n = list.length / 2
      val (head, tail) = list.splitAt(n)
    }
  }
}