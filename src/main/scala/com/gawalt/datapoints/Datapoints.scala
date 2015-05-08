package com.gawalt.datapoints

import scala.util.Try
import javax.swing.JFrame
import org.jfree.chart.{ChartFactory, ChartPanel}
import org.jfree.chart.plot.PlotOrientation
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}

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

  def plotColumn(idx: Int, numBins: Int, name: String = "") {
    val chartName = if (name.length == 0) s"Column $idx" else name

    val coll = new XYSeriesCollection()
    val data = new XYSeries(chartName)
    val xs = column(idx)
    val min = xs.min
    val max = xs.max
    val binWidth = (max - min)/numBins

    val arr = Array.fill[Int](numBins)(0)

    xs.foreach(x => {
      val bin = ((x - min)/binWidth).toInt
      if (bin > numBins - 1) arr(numBins - 1) += 1 else arr(bin) += 1
    })

    (0 until numBins).map(i => {
      val center = min + i*binWidth + binWidth/2
      data.add(center, arr(i))
    })

    coll.addSeries(data)

    val chart = ChartFactory.createXYLineChart(
      chartName, "Value", "Frequency",
      coll, PlotOrientation.VERTICAL,
      false, false, false)

    println(s"Displaying $chartName")

    val frame = new JFrame(s"$chartName Histogram")
    frame.setDefaultCloseOperation( JFrame.EXIT_ON_CLOSE )

    frame.setSize(640,420)
    frame.add( new ChartPanel(chart) )
    frame.pack()
    frame.setVisible(true)

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
    val rng = new scala.util.Random()
    val data = Datapoints.randn(10000, 40, rng)

    data.plotColumn(0, 10)

    data.plotColumn(1, 20)


  }

}