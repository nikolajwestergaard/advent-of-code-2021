package dk.versio.aoc2021

import scala.annotation.tailrec
import scala.io.Source

case class Target(x1: Int, x2: Int, y1: Int, y2: Int)

case class Probe(x: BigInt, y: BigInt, xv: BigInt, yv: BigInt)

object Dec17 extends App {
  val input = Source.fromResource("Dec17.txt").getLines().next().drop(13).split(", ")
    .flatMap(_.drop(2).split("\\.\\.")).map(_.toInt)
  val target = Target(input.head, input(1), input(2), input(3))

  @tailrec
  def travel(probe: Probe, max: BigInt = 0): Option[BigInt] = {
    var p = probe
    p = p.copy(x = p.x + p.xv)
    p = p.copy(y = p.y + p.yv)
    p = p.copy(xv = if (p.xv > 0) p.xv - 1 else 0)
    p = p.copy(yv = p.yv - 1)

    if (p.x >= target.x1 && p.x <= target.x2 && p.y >= target.y1 && p.y <= target.y2) // Within target
      return Some(max)

    if (p.x > target.x2 || p.y < target.y1) // Overshot
      return None

    travel(p, if (p.y > max) p.y else max)
  }

  val tries = 0.until(target.x2 + 1).flatMap(x => target.y1.until(target.y1.abs).flatMap(y => travel(Probe(0, 0, x, y))))
  println(s"Part 1: ${tries.max}")
  println(s"Part 2: ${tries.length}")
}