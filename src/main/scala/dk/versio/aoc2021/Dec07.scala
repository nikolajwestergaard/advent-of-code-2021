package dk.versio.aoc2021

import scala.annotation.tailrec
import scala.collection.immutable.List
import scala.io.Source

object Dec07 extends App {
  val counter = Source.fromResource("Dec07.txt").mkString.split(",").map(_.toInt)
  val fuelSpend = counter.min.until(counter.max).map(p => counter.map(f => (f - p).abs).sum)
  println(fuelSpend.min)
  val fuelSpendAdv = counter.min.until(counter.max).map(p => counter.map(f => 1.to((f - p).abs).sum).sum)
  println(fuelSpendAdv.min)
}