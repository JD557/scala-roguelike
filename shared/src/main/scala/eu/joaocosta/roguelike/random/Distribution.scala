package eu.joaocosta.roguelike.random

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

  def repeatN(n: Int): Distribution[List[T]] =
    if (n <= 0) Distribution.pure(Nil)
    else this.zipWith(repeatN(n - 1)) { case (x, xs) => x :: xs }
  def repeatUpToN(n: Int): Distribution[List[T]] =
    Distribution.range(n + 1).flatMap(repeatN)
}
object Distribution {
  final case class Sampler[T](sampler: Random => T) extends Distribution[T] {
    def sample(random: Random) = sampler(random)
  }
  final case class AndThen[T, U](x: Distribution[T], f: T => Distribution[U]) extends Distribution[U] {
    def sample(random: Random): U = f(x.sample(random)).sample(random)
  }

  def impure[T](f: Random => T): Distribution[T] = Sampler[T](f)
  def pure[T](x: T): Distribution[T]             = Sampler[T](_ => x)
  def uniform[T](hd: T, tl: T*): Distribution[T] = {
    val entries = hd +: tl.toVector
    impure { random =>
      entries(random.nextInt(entries.size))
    }
  }
  def range(toExcl: Int): Distribution[Int] =
    impure(_.nextInt(toExcl))
  def range(fromIncl: Int, toExcl: Int): Distribution[Int] =
    impure(_.between(fromIncl, toExcl))
  def weighted[T](hd: (Int, T), tl: (Int, T)*): Distribution[T] = {
    val preprocessedEntries: List[(Int, T)] =
      tl.scanLeft(hd) { case ((acc, _), (weight, x)) =>
        (acc + weight, x)
      }.toList
    val maxWeight = preprocessedEntries.last._1
    impure { random =>
      val roll = random.nextInt(maxWeight)
      preprocessedEntries.find(_._1 >= roll).getOrElse(preprocessedEntries.last)._2
    }
  }

  def traverse[T, U](xs: List[T])(f: T => Distribution[U]): Distribution[List[U]] =
    xs.foldRight(Distribution.pure(List.empty[U])) { case (d, ds) =>
      f(d).zipWith(ds)(_ :: _)
    }
  def sequence[T](xs: List[Distribution[T]]): Distribution[List[T]] = traverse(xs)(identity)
}
