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

  //@tailrec
  def train(data: Datapoints,
            labels: Seq[Int],
            stepSize: Double,
            init: Option[LogisticRegression] = None): LogisticRegression = {
    require(labels.length == data.m, "Must have as many labels as datapoints")
    require(stepSize > 0, "stepSize must be positive")

    val xy = data.points.zip(labels)

    val nMod = nullModel(data.n)

    def trainStep(current: LogisticRegression): LogisticRegression = {
      xy.foldLeft(nMod)({case (model, (xi, yi)) =>
        val lossWeightedStep = (current.predict(xi) - yi)*stepSize/data.m
        LogisticRegression(model.weights - xi.scale(lossWeightedStep),
          model.intercept - lossWeightedStep)
      })
    }

    var w = init.getOrElse(nMod)
    var prevLoss = w.loss(data, labels)
    var continue = true
    while (continue) {
      w = trainStep(w)
      if (w.hashCode() % 100 == 0) {
        val newLoss = w.loss(data, labels)
        if (newLoss > prevLoss)
          return train(data, labels, stepSize/2, Some(w))
        prevLoss = newLoss
        val grad = w.gradient(data, labels)
        if (grad.dot(grad) < 1e-4) continue = false
      }
    }

    w
  }
}