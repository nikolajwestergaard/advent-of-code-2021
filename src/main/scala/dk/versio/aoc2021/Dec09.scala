package dk.versio.aoc2021

import scala.annotation.tailrec
import scala.collection.immutable.List
import scala.io.Source

object Dec09 extends App {
  val input = Source.fromResource("Dec09.txt").getLines().map(_.map(_.toString.toInt).toList).toList
  val lp = input.zipWithIndex.flatMap(r => r._1.zipWithIndex.filter(c => (c._2 == 0 || input(r._2)(c._2 - 1) > c._1)
    && (c._2 == r._1.length - 1 || input(r._2)(c._2 + 1) > c._1)
    && (r._2 == 0 || input(r._2 - 1)(c._2) > c._1)
    && (r._2 == input.length - 1 || input(r._2 + 1)(c._2) > c._1)
  ).map(c => (c._1, (c._2, r._2))))
  println(lp.map(_._1 + 1).sum)

  def basinCalc(i: List[List[Int]], p: (Int, Int), v: List[(Int, Int)] = List()): List[(Int, Int)] =
    if (v.contains(p)
      || (p._1 < 0) || (p._1 > i.head.length - 1)
      || (p._2 < 0) || (p._2 > i.length - 1)
      || (input(p._2)(p._1) == 9)) v
    else List((1, 0), (-1, 0), (0, 1), (0, -1))
      .foldLeft(v)((m, d) => basinCalc(i, (p._1 + d._1, p._2 + d._2), m :+ p))

  val basins = lp.map(p => basinCalc(input, p._2))
  println(basins.map(_.distinct.length).sorted.takeRight(3).product)
}