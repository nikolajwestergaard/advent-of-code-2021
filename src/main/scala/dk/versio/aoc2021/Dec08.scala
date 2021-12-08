package dk.versio.aoc2021

import scala.annotation.tailrec
import scala.collection.immutable.List
import scala.io.Source

object Dec08 extends App {
  def findNumbers(n: List[String]) = {
    def diffs(s1: String, s2: String, n: Int) = s1.intersect(s2).length == n
    val s1 = n.find(_.length == 2).get
    val s3 = n.find(s => diffs(s, s1, 2) && s.length == 5).get
    val s4 = n.find(_.length == 4).get
    val s6 = n.find(s => diffs(s, s1, 1) && s.length == 6).get
    val s5 = n.find(s => diffs(s, s6, 5) && s.length == 5).get
    val s7 = n.find(_.length == 3).get
    val s8 = n.find(_.length == 7).get
    val s9 = n.find(s => diffs(s, s3, 5) && s.length == 6).get
    val s0 = n.find(s => diffs(s, s9, 5) && s.length == 6 && diffs(s, s1, 2)).get
    val s2 = n.find(s => diffs(s, s4, 2) && s.length == 5).get
    List(s0, s1, s2, s3, s4, s5, s6, s7, s8, s9)
  }

  val input = Source.fromResource("Dec08.txt").getLines().map(_.split(" \\| ").map(_.split(" ").toList).toList).toList
  println(input.map(_(1).count(l => l.length == 2 || l.length == 3 || l.length == 4 || l.length == 7)).sum)

  val res = input.map(p => p.last.map(pp => findNumbers(p.head).indexWhere(_.sorted == pp.sorted)).mkString.toInt)
  println(res.sum)
}