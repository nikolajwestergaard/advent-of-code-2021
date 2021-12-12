package dk.versio.aoc2021

import scala.annotation.tailrec
import scala.io.Source

object Dec11 extends App {
  type Cord = List[(Int, Int, Int)] // (X, Y, Level)
  val input: Cord = Source.fromResource("Dec11.txt")
    .getLines().toList.zipWithIndex.flatMap(l => l._1.zipWithIndex.map(c => (c._2, l._2, c._1.toString.toInt)))

  val dir = List((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1))

  @tailrec
  def flashes(i: Cord, ex: List[(Int, Int)] = List()): Cord = {
    val nfs = i.filter(o => o._3 > 9 && !ex.contains(o._1, o._2))
    if (nfs.isEmpty)
      return i

    var updated = i
    for (nf <- nfs) {
      for (d <- dir) {
        val p = i.indexWhere(o => o._1 == nf._1 + d._1 && o._2 == nf._2 + d._2)
        if (p >= 0)
          updated = updated.updated(p, updated(p).copy(_3 = updated(p)._3 + 1))
      }
    }
    flashes(updated, ex ++ nfs.map(o => (o._1, o._2)))
  }

  val steps = 0.to(100).foldLeft((input, 0))((m, i) => {
    val f = flashes(m._1)
    (f.map(o => if (o._3 > 9) o.copy(_3 = 0) else o).map(o => o.copy(_3 = o._3 + 1)), m._2 + f.count(_._3 > 9))
  })
  println(steps._2)

  @tailrec
  def findAllFlash(i: Cord, c: Int = 0): Int = {
    val f = flashes(i)
    if (f.count(_._3 > 9) == 100) c else findAllFlash(f.map(o => if (o._3 > 9) o.copy(_3 = 0) else o).map(o => o.copy(_3 = o._3 + 1)), c + 1)
  }

  println(findAllFlash(input))
}