package dk.versio.aoc2021

import scala.annotation.tailrec
import scala.io.Source

type NodeMap = Map[String, List[String]]
type Path = List[String]
type Strategy = (String, List[String]) => Boolean

object Dec12 extends App {
  val input = Source.fromResource("Dec12.txt").getLines().toList.map(_.split("-").toList)
  val map: NodeMap = (input ++ input.map(i => i.reverse)).groupBy(_.head).transform((_, v) => v.map(_.last))

  def part1strategy(n: String, v: List[String]): Boolean = n == n.toLowerCase && v.contains(n)

  def part2strategy(n: String, v: List[String]): Boolean = {
    val lc = v.filter(i => i.toLowerCase == i)
    (n == "start" && v.nonEmpty) || (n == n.toLowerCase && v.contains(n) && lc.distinct.size != lc.size)
  }

  def travel2(map: NodeMap, node: String, strategy: Strategy, visited: Path = List(), paths: List[Path] = List()): List[Path] = {
    if (node == "end") return paths :+ (visited :+ "end")
    if (strategy(node, visited)) return paths
    map(node).flatMap(next => travel2(map, next, strategy, visited :+ node, paths))
  }

  println(s"Part 1: ${travel2(map, "start", part1strategy).length}")
  println(s"Part 2: ${travel2(map, "start", part2strategy).length}")
}