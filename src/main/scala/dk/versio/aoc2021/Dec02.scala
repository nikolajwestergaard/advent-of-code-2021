package dk.versio.aoc2021

import scala.io.Source

object Dec02 extends App {
  def input = Source.fromResource("Dec02.txt").getLines().map(_.split(" ")).map(p => (p(0), p(1).toInt))

  val res1 = input.foldLeft((0, 0)) {
    case (m, ("forward", d)) => (m._1 + d, m._2)
    case (m, ("down", d)) => (m._1, m._2 + d)
    case (m, ("up", d)) => (m._1, m._2 - d)
  }
  println((res1, res1._1 * res1._2))

  val res2 = input.foldLeft((0, 0, 0)) {
    case (m, ("forward", d)) => m.copy(_1 = m._1 + d, _2 = m._2 + m._3 * d)
    case (m, ("down", d)) => m.copy(_3 = m._3 + d)
    case (m, ("up", d)) => m.copy(_3 = m._3 - d)
  }
  println((res2, res2._1 * res2._2))
}
