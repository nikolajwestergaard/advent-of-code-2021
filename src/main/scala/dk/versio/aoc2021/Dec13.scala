package dk.versio.aoc2021

import scala.io.Source

object Dec13 extends App {
  val input = Source.fromResource("Dec13.txt").getLines().toList.filter(_.nonEmpty)
  val cords = input.filter(l => l.head >= '0' && l.head <= '9').map(_.split(",")).map(p => (p.head.toInt, p(1).toInt))
  val folds: List[(String, Int)] = input.filter(_.head == 'f').map(_.split(" ").last.split("=")).map(f => (f.head, f(1).toInt))

  def fold(folds: List[(String, Int)]) = folds.foldLeft(cords) {
    case (i, ("x", x)) => i.map(p => if (p._1 > x) p.copy(_1 = x - (p._1 - x)) else p)
    case (i, ("y", y)) => i.map(p => if (p._2 > y) p.copy(_2 = y - (p._2 - y)) else p)
  }

  println(s"Part 1: ${fold(folds.take(1)).distinct.length}")
  println("Part 2:")
  val lastFold = fold(folds)
  0.to(lastFold.maxBy(_._2)._2).foreach(y =>
    println(0.to(lastFold.maxBy(_._1)._1).map(x => if (lastFold.contains((x, y))) "#" else " ").mkString))
}