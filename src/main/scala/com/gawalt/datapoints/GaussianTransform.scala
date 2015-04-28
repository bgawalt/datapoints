package com.gawalt.datapoints

/**
 * Created by brian on 4/25/15.
 */

/**
 * A collection of scalars ("raw data points") can all be transformed (e.g., squared,
 * exponenitated, or logarithm'd) and represented as coming from a Gaussian distribution.
 * The sufficient statistics (mean and standard deviation) of this distribution are stored along
 * the base transform to apply to future raw data points.
 * @param mean Mean of the data, post transformation
 * @param std Std. Deviation of the data, post transformation
 * @param baseTransform Function for transforming the raw data such that the resulting values are
 *                      more or less distributed as a normal curve
 */
case class GaussianTransform(mean: Double, std: Double, baseTransform: Double => Double) {
  def transform(x: Double) = (baseTransform(x) - mean)/std

  /**
   * (a) Map the raw data value to its transformed value, via baseTransform
   * (b) Map this transformed value to its analog z-Score, i.e., the analogous value it'd have
   * if this distribution were actually N(0, 1) instead of N(mean, std)
   * @param x Raw data point value to be transformed, first by baseTransform, then zScored
   * @return
   */
  def zScore(x: Double): Double = (baseTransform(x) - mean)/(2*std)

  private val BASE_LOG_LIKELIHOOD = -math.log(std) - math.log(2*math.Pi)/2

  /**
   * The gaussian likelihood function looks like:
   * p(x) = 1/[std*sqrt(2*PI)] exp{ -[(x - mean)/std]**2 }
   * log p(x) = - log{std} - 0.5 log{2PI} - [(x - mean)/std]**2
   * @param x Raw data point to be evaluated
   * @return Likelihood of this raw datapoint under the distribution represented by the
   *         Gaussian distribution corresponding to this GaussianTransform object
   */
  def logLikelihood(x: Double): Double = {
    val z = zScore(x)
    BASE_LOG_LIKELIHOOD - z*z
  }

  /**
   * Calculate the overall logLikelihood of all the given raw data values
   * @param xs Collection of raw data values
   * @return Overall likelihood of seeing this raw data values under this object's
   *         transformed-Gaussian
   */
  def logLikelihood(xs: Seq[Double]): Double =
    if (xs.isEmpty) 0
    else xs.map(logLikelihood).sum
}

object GaussianTransform {

  /**
   * Given the data points and a transformation to apply to them, fit the parameters of a
   * Gaussian distribution (mean and standard deviation) according to max. likelihood
   * @param xs Raw (untransformed) data points
   * @param baseTransform Transformation to apply to the raw data points (e.g., logarithm)
   * @return com.gawalt.datapoints.GaussianTransform object representing max-likelihood distribution
   */
  def fitDistribution(xs: Seq[Double], baseTransform: Double => Double): GaussianTransform = {
    var sum: Double = 0
    var sqSum: Double = 0
    val n = xs.length

    for (tx <- xs.map(baseTransform)) {
      // Try and combat overflow risk by dividing by m for each addend
      sum += tx/n
      sqSum += tx*tx/n
    }
    val mean = sum
    val std = math.sqrt((sqSum - mean*mean)*(n.toDouble/(n-1)))
    println(s"\t\t$mean, $std")
    GaussianTransform(mean = mean, std = std, baseTransform)
  }
}
