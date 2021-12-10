package dk.versio.aoc2021

import scala.io.Source

object Dec10 extends App {
  val input = Source.fromResource("Dec10.txt").getLines()

  def flip(c: Char) = c match {
    case '<' => '>'
    case '(' => ')'
    case '{' => '}'
    case '[' => ']'
  }

  def process(i: List[Char], s: List[Char] = List()): (Option[Char], List[Char]) =
    if (i.isEmpty) return (None, s)
    if (s.isEmpty) return process(i.drop(1), List(i.head))
    if (List('>', ')', '}', ']').contains(i.head) && i.head != flip(s.last)) return (Some(i.head), s)
    if (List('>', ')', '}', ']').contains(i.head))
      process(i.drop(1), s.dropRight(1))
    else
      process(i.drop(1), s :+ i.head)

  val res = input.toList.map(l => process(l.toList))
  val part1 = res.flatMap(_._1).map {
    case ')' => 3
    case ']' => 57
    case '}' => 1197
    case '>' => 25137
  }.sum
  println(s"Part 1: $part1")

  val unfinished = res.filter(_._1.isEmpty).map(_._2)
  val part2 = unfinished
    .map(_.reverse.foldLeft(0L)((m, n) => (m * 5) + List('(', '[', '{', '<').indexOf(n) + 1))
    .sorted.apply(unfinished.length / 2)
  println(s"Part 2: $part2")
}