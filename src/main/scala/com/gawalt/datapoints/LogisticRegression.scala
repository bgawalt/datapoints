package com.gawalt.datapoints

import scala.annotation.tailrec

/**
 * This source file created by Brian Gawalt, 7/9/15.
 * It is subject to the MIT license bundled with this package in the file LICENSE.txt.
 * Copyright (c) Brian Gawalt, 2015
 */
case class LogisticRegression(weights: Datum, intercept: Double) {

  def predict(x: Datum): Double = LogisticRegression.logit(weights.dot(x) + intercept)
  def predict(xs: Datapoints): IndexedSeq[Double] = xs.points.map(predict)

  // Once x and label have been validated
  private def cleanLoss(x: Datum, label: Int): Double = {
    val p = predict(x)
    -1*(label*math.log(p) + (1 - label)*math.log(1 - p))
  }

  def loss(x: Datum, label: Int): Double = {
    require(label == 0 || label == 1, "Label must be 0 or 1")
    cleanLoss(x, label)
  }
  def loss(xs: Datapoints, labels: Seq[Int]): Double = {
    require(labels.length == xs.m, "Number of labels doesn't match number of datapoints")
    require(labels.forall(y => y == 1 || y == 0), "All labels must be 1 or 0")
    xs.points.zip(labels)
      .map({case (x, y) => cleanLoss(x, y)/xs.m})
      .sum
  }

  def gradient(xs: Datapoints, labels: Seq[Int]): Datum = {
    xs.points.zip(labels).foldLeft(Datum.zero(xs.n))({case (g, (xi, yi)) =>
      val p = (predict(xi) - yi)/xs.m
      g - xi.scale(p)
    })
  }

}


object LogisticRegression {

  def nullModel(n: Int): LogisticRegression = {
    require(n > 0, "Model size must be positive")
    LogisticRegression(Datum.zero(n), 0.0)
  }

  def logit(z: Double): Double = 1.0/(1 + math.exp(-1*z))

  @tailrec
  def train(data: Datapoints,
            labels: Seq[Int],
            stepSize: Double,
            init: Option[LogisticRegression] = None,
            tol: Double = 1e-6): LogisticRegression = {
    require(labels.length == data.m, "Must have as many labels as datapoints")
    require(stepSize > 0, "stepSize must be positive")

    val xy = data.points.zip(labels)

    val nMod = nullModel(data.n)

    def trainStep(current: LogisticRegression): LogisticRegression = {
      val update =
        xy.foldLeft((Datum.zero(current.weights.n), 0.0))({case ((wAcc, bAcc), (xi, yi)) =>
        val lossWeightedStep = (current.predict(xi) - yi)*stepSize/data.m
        (wAcc + xi.scale(lossWeightedStep), bAcc + lossWeightedStep)
      })
      current.copy(weights = current.weights - update._1, intercept = current.intercept - update._2)
    }

    var w = init.getOrElse(nMod)
    var prevLoss = w.loss(data, labels)
    var continue = true
    var smallerStepRequired = false
    var iter = 0
    while (continue) {
      iter +=1
      w = trainStep(w)
      if (iter % 10 == 0) {
        val newLoss = w.loss(data, labels)
        if (newLoss > prevLoss || newLoss.isNaN) {
          smallerStepRequired = true
          continue = false
        }
        prevLoss = newLoss
        val grad = w.gradient(data, labels)
        if (grad.dot(grad) < tol*tol) continue = false
        println(s"Iter $iter: $newLoss, ${grad.dot(grad)}")
      }
    }

    if (smallerStepRequired) {
      println(s"Shrinking step size from $stepSize")
      if (w.weights.elements.exists(wj => wj.isNaN) || w.intercept.isNaN)
        train(data, labels, stepSize/2) // Just start from scratch if we've gone into NaN-land
      else train(data, labels, stepSize/2, Some(w))
    }
    else w
  }

  def main(args: Array[String]) {
    val rng = new scala.util.Random()
    val nPoints = 10000
    val A = 2.0
    val B = -1.5

    val xy = (0 until nPoints).map(i => {
      val xi = rng.nextGaussian()
      val zi = B + A*xi
      val p = rng.nextDouble()
      val yi = if (p < logit(zi)) 1 else 0
      (xi, yi)
    })

    val xyUnzip = xy.unzip
    val data = Datapoints(xyUnzip._1.map(xi => Datum(Vector(xi))))
    val labels = xyUnzip._2

    val mod = train(data, labels, 1000.0)

    println(mod.weights.elements.mkString(", "))
    println(mod.intercept)

  }
}