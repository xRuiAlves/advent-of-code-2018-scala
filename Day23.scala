//> using scala "3.3.0"
//> using jvm "temurin:17"
//> using file util/ResourceUtils.scala
//> using resourceDir inputs

import util.ResourceUtils.readResourceLines

import scala.annotation.tailrec
import scala.collection.mutable

object Day23 {
  def main(args: Array[String]): Unit = {
    val input = readResourceLines("day23.txt")
    val nanobots = input.map(Nanobot.fromStr)
    val maxRadiusNanobot = nanobots.maxBy(_.radius)

    val part1 = nanobots.count(maxRadiusNanobot.isInRange)
    val part2 = 0

    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
  }

  case class Nanobot(x: Long, y: Long, z: Long, radius: Long) {
    def isInRange(other: Nanobot): Boolean =
      math.abs(x - other.x) + math.abs(y - other.y) + math.abs(z - other.z) <= radius
  }

  object Nanobot {
    def fromStr(nanobotStr: String): Nanobot = nanobotStr match {
      case s"pos=<$x,$y,$z>, r=$radius" => Nanobot(x.toLong, y.toLong, z.toLong, radius.toLong)
    }
  }
}
