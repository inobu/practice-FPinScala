import datastructures.List.{flatMap2,flatMap}
import datastructures.{Cons, List, Nil}

object Main extends App {
  val x = flatMap2(List(1, 2, 3))((a) => List(a ,a))
  val y = flatMap(List(1, 2, 3))((a) => List(a ,a))

  println(x)
  println(y)
}
