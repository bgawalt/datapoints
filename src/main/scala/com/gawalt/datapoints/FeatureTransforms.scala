package com.gawalt.datapoints

/**
 * This source file created by Brian Gawalt, 4/25/15.
 * It is subject to the MIT license bundled with this package in the file LICENSE.txt.
 * Copyright (c) Brian Gawalt, 2015
 */

object FeatureTransforms {

  val noop: Double => Double = x => x
  val log: Double => Double = x => if (x > 0) math.log(x) else Double.NegativeInfinity

  val standards = List(noop, log)

}
