package com.broilogabriel

import scala.io.Source

/**
  * Created by broilogabriel on 5/18/2017.
  */
object Main extends App {

  val rollingWindow = 60

  if (args.nonEmpty) {
    println("T          V       N RS      MinV    MaxV")
    println("---------------------------------------------")
    Source.fromFile(args.head).getLines()
      .map(line => {
        val list = line.split("[ \\t]")
        Measure(list.head.toLong, list.last.toDouble)
      })
      .foldLeft(Analyzed(0, 0, List())) {
        (acc, elem) => {
          val measures = acc.measures.filter(_.timestamp > elem.timestamp - rollingWindow) :+ elem
          val analyzed = Analyzed(elem.timestamp, elem.price, measures)
          println(analyzed)
          analyzed
        }
      }
  }

}

case class Measure(timestamp: Long, price: Double) {
  override def toString: String = s"$timestamp $price"
}

case class Analyzed(timestamp: Long, price: Double, measures: List[Measure]) {
  def minMaxSum: (Double, Double, Double) = {
    measures.foldLeft((Double.MaxValue, 0D, 0D)) {
      (acc, e) => (math.min(acc._1, e.price), math.max(acc._2, e.price), acc._3 + e.price)
    }
  }

  override def toString: String = {
    val (min, max, sum) = this.minMaxSum
    f"$timestamp $price ${measures.size} $sum%.5f $min $max"
  }
}