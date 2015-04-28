package com.gawalt.datapoints

/**
 * Created by brian on 4/25/15.
 */
object FeatureTransforms {

  val noop: Double => Double = x => x
  val log: Double => Double = x => if (x > 0) math.log(x) else Double.NegativeInfinity

  val standards = List(noop, log)

}
