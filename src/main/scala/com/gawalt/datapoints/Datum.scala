package com.gawalt.datapoints

/**
 * This source file created by Brian Gawalt, 4/25/15.
 * It is subject to the MIT license bundled with this package in the file LICENSE.txt.
 * Copyright (c) Brian Gawalt, 2015
 */

case class Datum(private val elements: IndexedSeq[Double]) {
  val n = elements.size
  require(n > 0, "Can't create a zero-dimensional datapoint")

  def toCSVrow: String = elements.mkString(",")

  def dot(other: Datum): Double = {
    require(n == other.n,
      s"Can't take inner product of two data points of different lengths ($n v. ${other.n})")
    elements.zip(other.elements).map({case (xi, yi) => xi * yi}).sum
  }

  def apply(idx: Int): Double = elements(idx)
}

object Datum {
  def fromCSVrow(line: String) = Datum(line.split(",").map(_.toDouble).toVector)
  /** Generate n random feature values, uniformly between 0 and 1 */
  def random(n: Int, rng: scala.util.Random): Datum = {
    require(n > 0, "Number of requested features must be positive")
    Datum(Vector.fill[Double](n)(rng.nextDouble()))
  }
  /** Generate n random feature values, drawn from a N(0, 1) normal distribution */
  def randn(n: Int, rng: scala.util.Random): Datum = {
    require(n > 0, "Number of requested features must be positive")
    Datum(Vector.fill[Double](n)(rng.nextGaussian()))
  }
}
