package eu.joaocosta.roguelike.generator

import scala.annotation.tailrec
import scala.util.Random

import eu.joaocosta.roguelike._

sealed trait Room {
  def tiles(): Iterator[(Int, Int)]
  def intersects(that: Room): Boolean = {
    val thisSet = this.tiles().toSet
    that.tiles().exists(thisSet)
  }
}

object Room {
  case class RectangularRoom(x: Int, y: Int, width: Int, height: Int) extends Room {
    lazy val center = (x + width / 2, y + height / 2)
    def tiles(): Iterator[(Int, Int)] =
      for {
        dx <- (1 until width).iterator
        dy <- (1 until height).iterator
      } yield ((x + dx), (y + dy))
  }

  case class TunnelA(x1: Int, y1: Int, x2: Int, y2: Int) extends Room {
    def tiles(): Iterator[(Int, Int)] = {
      val horizontalLine = (x1 to x2 by math.signum(x2 - x1)).iterator.map(x => (x, y1))
      val verticalLine   = (y1 to y2 by math.signum(y2 - y1)).iterator.map(y => (x2, y))
      horizontalLine ++ verticalLine
    }
  }

  case class TunnelB(x1: Int, y1: Int, x2: Int, y2: Int) extends Room {
    def tiles(): Iterator[(Int, Int)] = {
      val verticalLine   = (y1 to y2 by math.signum(y2 - y1)).iterator.map(y => (x1, y))
      val horizontalLine = (x1 to x2 by math.signum(x2 - x1)).iterator.map(x => (x, y2))
      verticalLine ++ horizontalLine
    }
  }
}
