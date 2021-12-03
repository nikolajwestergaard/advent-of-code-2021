package dk.versio.aoc2021

import scala.annotation.tailrec
import scala.io.Source

object Dec03 extends App {
  val input = Source.fromResource("Dec03.txt").getLines().toList

  def binToInt(s: String) = Integer.parseInt(s.mkString, 2)

  def bitOrder(in: List[String], strategy: (Int, Float) => Boolean) =
    0.until(in.head.length).map(p => if (strategy(in.count(_ (p) == '1'), in.length.toFloat / 2)) '1' else '0').mkString

  @tailrec
  def bitOrderAndFilter(in: List[String], strategy: (Int, Float) => Boolean, p: Int = 0): String =
    if (in.length <= 1) in.head else bitOrderAndFilter(in.filter(_ (p) == bitOrder(in, strategy)(p)), strategy, p + 1)

  println(binToInt(bitOrder(input, _ >= _)) * binToInt(bitOrder(input, _ < _)))
  println(binToInt(bitOrderAndFilter(input, _ >= _)) * binToInt(bitOrderAndFilter(input, _ < _)))
}
