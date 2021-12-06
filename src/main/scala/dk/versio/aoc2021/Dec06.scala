package dk.versio.aoc2021

import scala.annotation.tailrec
import scala.collection.immutable.List
import scala.io.Source

object Dec06 extends App {

  @tailrec
  def breed(counters: List[Long], timespan: Long): List[Long] = if (timespan == 0) counters else
    breed(counters.updated(7, counters(7) + counters.head).drop(1) :+ counters.head, timespan - 1)

  val counter = Source.fromResource("Dec06.txt").mkString.split(",").map(_.toInt)
    .foldLeft(List.fill(9)(0))((m, p) => m.updated(p, m(p) + 1)).map(_.toLong)

  println(breed(counter, 80).sum)
  println(breed(counter, 256).sum)
}