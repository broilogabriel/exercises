package com.broilogabriel

import org.slf4j.LoggerFactory

import scala.io.Source

/**
  * Created by broilogabriel on 5/18/2017.
  */
object Main extends App {

  val rollingWindow = 60
  val logger = LoggerFactory.getLogger(this.getClass)

  if (args.nonEmpty) {
    logger.info("T          V       N RS      MinV    MaxV")
    logger.info("---------------------------------------------")
    Source.fromFile(args.head).getLines()
      .map(line => {
        val list = line.split("[ \\t]")
        Measure(list.head.toLong, list.last.toDouble)
      })
      // After the data is normalized the calculation is done using foldLeft as an accumulator
      .foldLeft(Analyzed(0, 0, List())) {
      (acc, elem) => {
        val measures = acc.recentMeasures(elem.timestamp - rollingWindow) :+ elem
        val analyzed = Analyzed(elem.timestamp, elem.price, measures)
        logger.info(analyzed.toString)
        analyzed
      }
    }
  } else {
    throw new Exception("Missing file path argument.")
  }

}

/**
  * Represents the input measure
  *
  * @param timestamp epoch time
  * @param price     with precision 5
  */
case class Measure(timestamp: Long, price: Double)

/**
  * Represents the analyzed data, the min, max and sum are calculated when needed only
  *
  * @param timestamp epoch time
  * @param price     with precision 5
  * @param measures  list of measures to be analyzed
  */
case class Analyzed(timestamp: Long, price: Double, measures: List[Measure]) {

  /**
    * Calculates the min, max and sum of the list of measures
    *
    * @return (min, max, sum)
    */
  def minMaxSum: (Double, Double, Double) = {
    measures.foldLeft((Double.MaxValue, 0D, 0D)) {
      (acc, e) => (math.min(acc._1, e.price), math.max(acc._2, e.price), acc._3 + e.price)
    }
  }

  /**
    * Returns a filtered list with measures greater than the specified time, this method is not really needed at the
    * moment due it's simplicity
    *
    * @param time epoch time
    * @return filtered list of Measure
    */
  def recentMeasures(time: Long): List[Measure] = {
    measures.filter(_.timestamp > time)
  }

  override def toString: String = {
    val (min, max, sum) = this.minMaxSum
    f"$timestamp $price ${measures.size} $sum%.5f $min $max"
  }
}