package dk.versio.aoc2021

import scala.io.Source

case class Packet(v: BigInt, t: BigInt, cv: BigInt, l: Int = 0, lit: BigInt = -1, children: List[Packet] = List())

object Dec16 extends App {
  def toLong(l: List[Char]): BigInt = BigInt(l.mkString, 2)

  val input: List[Char] = Source.fromResource("Dec16.txt").getLines().toList.head
    .map(c => Integer.parseInt(c.toString, 16)).toList
    .flatMap(_.toBinaryString.toList.reverse.padTo(4, '0').reverse)

  def parsePacket(i: List[Char]): Packet = {
    val v = toLong(i.slice(0, 3))
    val t = toLong(i.slice(3, 6))
    if (t == 4) {
      val s = i.drop(6).sliding(5, 5).toList
      val last = s.indexWhere(_.head == '0')
      val lit = toLong(s.take(last + 1).flatMap(_.drop(1)))
      Packet(v, t, v, 6 + 5 * (last + 1), lit)
    } else {
      val l15 = i(6) == '0'
      val sl = if (l15) toLong(i.slice(7, 22)) else toLong(i.slice(7, 18))
      var pgs: List[Packet] = List()
      var total = 0
      val spi = i.drop(if (l15) 22 else 18)
      while (total < sl) {
        pgs = pgs :+ parsePacket(spi.drop(pgs.map(_.l).sum))
        total = if (l15) pgs.map(_.l).sum else pgs.length
      }
      Packet(v, t, v + pgs.map(_.cv).sum, children = pgs, l = (if (l15) 22 else 18) + pgs.map(_.l).sum)
    }
  }

  def calcValue(p: Packet): BigInt = p match {
    case Packet(_, 0, _, _, _, children) => children.map(calcValue).sum
    case Packet(_, 1, _, _, _, children) => children.map(calcValue).product
    case Packet(_, 2, _, _, _, children) => children.map(calcValue).min
    case Packet(_, 3, _, _, _, children) => children.map(calcValue).max
    case Packet(_, 4, _, _, lit, children) => lit
    case Packet(_, 5, _, _, _, children) => if (calcValue(children.head) > calcValue(children.last)) 1 else 0
    case Packet(_, 6, _, _, _, children) => if (calcValue(children.head) < calcValue(children.last)) 1 else 0
    case Packet(_, 7, _, _, _, children) => if (calcValue(children.head) == calcValue(children.last)) 1 else 0
  }

  val packet = parsePacket(input)
  println(s"Part 1: ${packet.cv}")
  println(s"Part 2: ${calcValue(packet)}")
}