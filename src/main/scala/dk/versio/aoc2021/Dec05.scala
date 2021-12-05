package dk.versio.aoc2021

import scala.annotation.tailrec
import scala.collection.immutable.List
import scala.io.Source

object Dec05 extends App {

  @tailrec
  def draw(x1: Int, y1: Int, x2: Int, y2: Int, points: List[(Int, Int)] = List()): List[(Int, Int)] =
    if (x1 == x2 && y1 == y2) points :+ (x1, y1) else
      draw(x1 - x1.compare(x2), y1 - y1.compare(y2), x2, y2, points :+ (x1, y1))

  val input = Source.fromResource("Dec05.txt").getLines()
    .map(_.split(" -> ").map(_.split(",").map(_.toInt)))
    .map(p => (p.head.head, p.head(1), p(1).head, p(1)(1))).toList

  val straightLines = input.filter((x1, y1, x2, y2) => x1 == x2 || y1 == y2)
    .flatMap((x1, y1, x2, y2) => draw(x1, y1, x2, y2)).toList
  println(straightLines.groupBy(_.toString()).count(_._2.length >= 2))

  val withDiagonals = input.flatMap((x1, y1, x2, y2) => draw(x1, y1, x2, y2)).toList
  println(withDiagonals.groupBy(_.toString()).count(_._2.length >= 2))
}