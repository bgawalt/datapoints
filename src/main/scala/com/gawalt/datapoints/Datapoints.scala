package com.gawalt.datapoints

import scala.util.Try

/**
 * This source file created by Brian Gawalt, 4/25/15.
 * It is subject to the MIT license bundled with this package in the file LICENSE.txt.
 * Copyright (c) Brian Gawalt, 2015
 */

/** A simple container for collecting a family of points in some real-valued feature space */
case class Datapoints(points: IndexedSeq[Datum]) {
  /** Number of features */
  val n = Try(points.head.n).getOrElse(0)
  require(points.forall(_.n == n))

  /** Number of data points */
  val m = points.length

  /** Take the inner product of the given datapoint with all points in this dataset */
  def dot(v: Datum): Datum = Datum(points.map(p => p.dot(v)))

  /** Extract the feature values at the given index from all points in dataset */
  def column(idx: Int): IndexedSeq[Double] = {
    require(idx >= 0 && idx < n, s"Index must be between 0 and n-1 (= ${n-1} for this Data object)")
    points.map(p => p(idx))
  }

  /**
   *
   * @param transforms Candidate transformations to attempt on each column of this dataset
   * @return Best-fitting GaussianTransforms for each column, as selected from the candidate set
   */
  def fitTransformedGaussians(transforms: Seq[Double => Double]): Seq[GaussianTransform] = {
    (0 until n).map(idx => {
      val col = column(idx)
      var bestGT: GaussianTransform = null
      var bestLL = Double.NegativeInfinity
      for (transform <- transforms) {
        val gt = GaussianTransform.fitDistribution(col, transform)
        val ll = gt.logLikelihood(col)
        println(s"\t$ll")
        if (ll > bestLL) {
          bestLL = ll
          bestGT = gt
        }
      }
      println(s"Preferring 2 => ${bestGT.baseTransform(2)}")
      bestGT
    })
  }
}

object Datapoints {
  def random(m: Int, n: Int, rng: scala.util.Random): Datapoints = {
    Datapoints((0 until m).map(i => Datum.random(n, rng)))
  }
  def randn(m: Int, n: Int, rng: scala.util.Random): Datapoints = {
    Datapoints((0 until m).map(i => Datum.randn(n, rng)))
  }

  def main(args: Array[String]) {
    val m = 10000
    val rng = new scala.util.Random(System.nanoTime())
    val data = Datapoints(
      (0 until m).map(i =>  Datum(Vector(
          5 + 4*rng.nextGaussian(),
          1 + 2*rng.nextGaussian(),
          math.pow(3 + 4*rng.nextGaussian(), 2),
          math.exp(5 + 6*rng.nextGaussian())
        ))
      ))

    val gts = data.fitTransformedGaussians(FeatureTransforms.standards)
    println(data.column(0).sum)

    gts.foreach(println)
  }

}