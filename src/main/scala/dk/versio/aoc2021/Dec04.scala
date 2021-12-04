package dk.versio.aoc2021

import scala.annotation.tailrec
import scala.io.Source

object Bingo {
  @tailrec
  def draw(draws: List[Int], boards: List[List[List[Int]]]): (List[List[Int]], Int) = {
    val updated = boards.map(_.map(_.filter(_ != draws.head)))
    val winner = updated.indexWhere(_.exists(_.isEmpty))
    if (winner >= 0) (updated(winner), draws.head) else draw(draws.drop(1), updated)
  }

  @tailrec
  def drawLoser(draws: List[Int], boards: List[List[List[Int]]]): (List[List[Int]], Int) = {
    val updated = boards.map(_.map(_.filter(_ != draws.head)))
    val losers = updated.count(!_.exists(_.isEmpty))
    if (losers == 0) (updated(boards.indexWhere(!_.exists(_.isEmpty))), draws.head) else
      drawLoser(draws.drop(1), updated)
  }
}

object Dec04 extends App {
  val input = Source.fromResource("Dec04.txt").getLines().toList
  val draws = input.head.split(",").map(_.toInt).toList
  val boards = input.drop(2)
    .filter(_.nonEmpty)
    .sliding(5, 5)
    .map(_.map(_.split(" ").filter(_.nonEmpty).map(_.toInt).toList))
    .map(b => b ++ 0.until(5).map(p => b.map(_ (p))))
    .toList

  val winner = Bingo.draw(draws, boards)
  println(winner._1.map(_.sum).take(5).sum * winner._2)

  val looser = Bingo.drawLoser(draws, boards)
  println(looser._1.map(_.sum).take(5).sum * looser._2)
}
