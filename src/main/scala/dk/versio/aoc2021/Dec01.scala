package dk.versio.aoc2021

import scala.io.Source

object Dec01 extends App {
  def input = Source.fromResource("Dec01.txt").getLines().map(_.toInt)
  println(input.sliding(2).count(m => m.head < m(1)))
  println(input.sliding(3).map(_.sum).sliding(2).count(m => m.head < m(1)))
}
