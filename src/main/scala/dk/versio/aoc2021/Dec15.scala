package dk.versio.aoc2021

import dk.versio.aoc2021.Dec15.grid2

import scala.collection.mutable
import scala.io.Source

case class Cell(x: Int, y: Int, d: Int)

object Dec15 extends App {
  val grid1 = Source.fromResource("Dec15.txt").getLines().toList.map(_.toList.map(_.toString.toInt))
  val grid2: List[List[Int]] = 0.until(5)
    .flatMap(r => grid1.map(_.map(n => (n + r - 1) % 9 + 1)))
    .map(r => 0.until(5).flatMap(c => r.map(n => (n + c - 1) % 9 + 1)).toList).toList
  val dx = List(-1, 0, 1, 0)
  val dy = List(0, 1, 0, -1)

  def cheapestPath(g: List[List[Int]]) = {
    val dim = g.length
    val dis = mutable.ArrayBuffer.fill(dim, dim)(10000000)
    dis(0)(0) = g.head.head

    def insideGrid = (x: Int, y: Int) => x >= 0 && x < dim && y >= 0 && y < dim

    val pp = mutable.PriorityQueue(Cell(0, 0, 0))((a, b) => if (a.d < b.d) 1 else if (a.d > b.d) -1 else 0)
    while (pp.nonEmpty) {
      val k = pp.dequeue()
      for (i <- 0.until(4)) {
        val x = k.x + dx(i)
        val y = k.y + dy(i)
        if (insideGrid(x, y) && dis(x)(y) > dis(k.x)(k.y) + g(x)(y)) {
          dis(x)(y) = dis(k.x)(k.y) + g(x)(y)
          pp.enqueue(Cell(x, y, dis(x)(y)))
        }
      }
    }
    dis.last.last - g.head.head
  }

  println(s"Part 1: ${cheapestPath(grid1)}")
  println(s"Part 2: ${cheapestPath(grid2)}")
}