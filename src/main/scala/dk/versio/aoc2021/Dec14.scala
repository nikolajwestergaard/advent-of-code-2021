package dk.versio.aoc2021

import scala.collection.mutable
import scala.io.Source

object Dec14 extends App {
  val input = Source.fromResource("Dec14.txt").getLines().toList
  val start: List[String] = input.head.sliding(2).toList
  val lookup: Map[String, (Char, Long)] = input.drop(2).map(_.split(" -> "))
    .map(l => l.head -> (l(1).head, start.count(_ == l.head).toLong)).toMap

  def calComb(steps: Int): Map[Char, Long] = 0.until(steps).foldLeft(lookup)((l, n) =>
    l.foldLeft(l)((n, f) => {
      val head = s"${f._1.head}${f._2._1}"
      val tail = s"${f._2._1}${f._1.last}"
      Option(n)
        .map(n => n.updated(f._1, f._2.copy(_2 = n(f._1)._2 - f._2._2)))
        .map(n => n.updated(head, l(head).copy(_2 = n(head)._2 + f._2._2)))
        .map(n => n.updated(tail, l(tail).copy(_2 = n(tail)._2 + f._2._2))).get
    })).groupBy(_._1.last).transform((c, m) => m.map(_._2._2).sum)

  val p1 = calComb(10)
  println(p1.maxBy(_._2)._2 - p1.minBy(_._2)._2)

  val r2 = calComb(40)
  println(r2.maxBy(_._2)._2 - r2.minBy(_._2)._2)
}