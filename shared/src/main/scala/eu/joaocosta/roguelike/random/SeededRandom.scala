package eu.joaocosta.roguelike.random

case class SeededRandom(seed: Long) {
  lazy val nextSeed = (seed * 0x5deece66dL + 0xbL) & ((1L << 48) - 1)
  def nextInt(): (SeededRandom, Int) =
    (SeededRandom(nextSeed), (seed >>> 16).toInt)
  def nextInt(limit: Int): (SeededRandom, Int) = {
    val (rng, int) = nextInt()
    val mod        = int % limit
    if (int - (limit - 1) >= 0) (rng, mod) else rng.nextInt(limit)
  }
}
