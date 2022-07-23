package eu.joaocosta.roguelike.generator

import scala.annotation.tailrec

import eu.joaocosta.roguelike._

sealed trait Room {
  def tiles: Set[(Int, Int)]
  lazy val outerTiles: Set[(Int, Int)] =
    for {
      (x, y) <- tiles
      dx     <- -1 to 1
      dy     <- -1 to 1
    } yield (x + dx, y + dy)
  def intersects(that: Room): Boolean =
    this.outerTiles.exists(that.outerTiles)
}

object Room {
  case class RectangularRoom(x: Int, y: Int, width: Int, height: Int) extends Room {
    lazy val center = (x + width / 2, y + height / 2)
    lazy val tiles: Set[(Int, Int)] = new Set[(Int, Int)] {
      def contains(elem: (Int, Int)): Boolean =
        elem._1 >= 1 && elem._1 < width && elem._2 >= 1 && elem._2 < height
      def iterator: Iterator[(Int, Int)] = (for {
        dx <- (1 until width).iterator
        dy <- (1 until height).iterator
      } yield ((x + dx), (y + dy)))
      def incl(elem: (Int, Int)): Set[(Int, Int)] =
        if (contains(elem)) this
        else iterator.toSet.incl(elem)
      def excl(elem: (Int, Int)): Set[(Int, Int)] =
        if (!contains(elem)) this
        else iterator.toSet.excl(elem)
    }
  }

  case class TunnelA(x1: Int, y1: Int, x2: Int, y2: Int) extends Room {
    lazy val tiles: Set[(Int, Int)] = {
      val hSign          = math.signum(x2 - x1)
      val vSign          = math.signum(y2 - y1)
      val horizontalLine = if (hSign == 0) Iterator.empty else (x1 to x2 by hSign).iterator.map(x => (x, y1))
      val verticalLine   = if (vSign == 0) Iterator.empty else (y1 to y2 by vSign).iterator.map(y => (x2, y))
      (horizontalLine ++ verticalLine).toSet
    }
  }

  case class TunnelB(x1: Int, y1: Int, x2: Int, y2: Int) extends Room {
    lazy val tiles: Set[(Int, Int)] = {
      val hSign          = math.signum(x2 - x1)
      val vSign          = math.signum(y2 - y1)
      val verticalLine   = if (vSign == 0) Iterator.empty else (y1 to y2 by vSign).iterator.map(y => (x1, y))
      val horizontalLine = if (hSign == 0) Iterator.empty else (x1 to x2 by hSign).iterator.map(x => (x, y2))
      (verticalLine ++ horizontalLine).toSet
    }
  }
}
