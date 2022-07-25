package eu.joaocosta.roguelike.generator

import scala.util.Random

sealed trait Distribution[+T] {
  def sample(random: Random): T
  def sampleN(n: Int, random: Random): List[T]     = List.fill(n)(sample(random))
  def sampleUpToN(n: Int, random: Random): List[T] = List.fill(random.nextInt(n + 1))(sample(random))

  def map[U](f: T => U): Distribution[U]                   = flatMap(x => Distribution.pure(f(x)))
  def flatMap[U](f: T => Distribution[U]): Distribution[U] = Distribution.AndThen[T, U](this, f)
  def zipWith[U, V](that: Distribution[U])(f: (T, U) => V): Distribution[V] =
    this.flatMap { t => that.map(u => f(t, u)) }
  def ap[V](that: Distribution[T => V]): Distribution[V] =
    zipWith(that) { case (x, f) => f(x) }
}
object Distribution {
  final case class Constant[T](entry: T) extends Distribution[T] {
    def sample(random: Random) = entry
  }
  final case class Uniform[T](entries: Vector[T]) extends Distribution[T] {
    def sample(random: Random) = entries(random.nextInt(entries.size))
  }
  final case class Range(fromIncl: Int, toExcl: Int) extends Distribution[Int] {
    def sample(random: Random) = random.between(fromIncl, toExcl)
  }
  final case class Weighted[T](entries: List[(Int, T)]) extends Distribution[T] {
    val preprocessedEntries: List[(Int, T)] =
      entries.tail.scanLeft(entries.head) { case ((acc, _), (weight, x)) =>
        (acc + weight, x)
      }
    val maxWeight = preprocessedEntries.last._1
    def sample(random: Random): T = {
      val roll = random.nextInt(maxWeight)
      preprocessedEntries.find(_._1 >= roll).getOrElse(preprocessedEntries.last)._2
    }
  }
  final case class AndThen[T, U](x: Distribution[T], f: T => Distribution[U]) extends Distribution[U] {
    def sample(random: Random): U = f(x.sample(random)).sample(random)
  }

  def pure[T](x: T): Distribution[T] = Constant[T](x)
  def uniform[T](hd: T, tl: T*): Distribution[T] =
    new Uniform(hd +: tl.toVector)
  def range(fromIncl: Int, toExcl: Int): Distribution[Int] =
    new Range(fromIncl, toExcl)
  def range(toExcl: Int): Distribution[Int] =
    new Range(0, toExcl)
  def weighted[T](hd: (Int, T), tl: (Int, T)*): Distribution[T] =
    new Weighted[T](hd :: tl.toList)
}
