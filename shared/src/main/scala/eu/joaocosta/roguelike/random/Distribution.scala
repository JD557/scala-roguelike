package eu.joaocosta.roguelike.random

sealed trait Distribution[+T] {
  def sample(random: SeededRandom): (SeededRandom, T)
  def sampleN(n: Int, random: SeededRandom): (SeededRandom, List[T]) =
    repeatN(n).sample(random)
  def sampleUpToN(n: Int, random: SeededRandom): (SeededRandom, List[T]) =
    repeatUpToN(n).sample(random)

  def map[U](f: T => U): Distribution[U]                   = flatMap(x => Distribution.pure(f(x)))
  def flatMap[U](f: T => Distribution[U]): Distribution[U] = Distribution.AndThen[T, U](this, f)
  def zipWith[U, V](that: Distribution[U])(f: (T, U) => V): Distribution[V] =
    this.flatMap { t => that.map(u => f(t, u)) }
  def ap[V](that: Distribution[T => V]): Distribution[V] =
    zipWith(that) { case (x, f) => f(x) }

  def repeatN(n: Int): Distribution[List[T]] =
    Distribution.sequence(List.fill(n)(this))
  def repeatUpToN(n: Int): Distribution[List[T]] =
    Distribution.range(n + 1).flatMap(repeatN)
}
object Distribution {
  final case class Sampler[T](sampler: SeededRandom => (SeededRandom, T)) extends Distribution[T] {
    def sample(random: SeededRandom) = sampler(random)
  }
  final case class AndThen[T, U](x: Distribution[T], f: T => Distribution[U]) extends Distribution[U] {
    def sample(random: SeededRandom) = {
      val (rng1, resX) = x.sample(random)
      f(resX).sample(rng1)
    }
  }

  def pure[T](x: T): Distribution[T]        = Sampler[T](rng => (rng, x))
  def range(toExcl: Int): Distribution[Int] = Sampler[Int](_.nextInt(toExcl))
  def range(fromIncl: Int, toExcl: Int): Distribution[Int] =
    range(toExcl - fromIncl).map(_ + fromIncl)
  def uniform[T](hd: T, tl: T*): Distribution[T] = {
    val entries = hd +: tl.toVector
    range(entries.size).map(entries.apply)
  }
  def weighted[T](hd: (Int, T), tl: (Int, T)*): Distribution[T] = {
    val preprocessedEntries: List[(Int, T)] =
      tl.scanLeft(hd) { case ((acc, _), (weight, x)) =>
        (acc + weight, x)
      }.toList
    val maxWeight = preprocessedEntries.last._1
    range(maxWeight).map { roll =>
      preprocessedEntries.find(_._1 >= roll).getOrElse(preprocessedEntries.last)._2
    }
  }

  def traverse[T, U](xs: List[T])(f: T => Distribution[U]): Distribution[List[U]] =
    xs.foldRight(Distribution.pure(List.empty[U])) { case (d, ds) =>
      f(d).zipWith(ds)(_ :: _)
    }
  def sequence[T](xs: List[Distribution[T]]): Distribution[List[T]] = traverse(xs)(identity)
}
