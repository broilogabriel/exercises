package com.broilogabriel

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by broilogabriel on 5/19/2017.
  */
class MainTest extends FlatSpec with Matchers {

  "An empty Analyzed object" should "returns a tuple for minMaxSum with default values" in {
    val analyzed = Analyzed(1L, 1D, List())
    analyzed.minMaxSum should be(Double.MaxValue, 0D, 0D)
  }

  "An Analyzed object with one value" should "returns a tuple for minMaxSum with the same value for all" in {
    val price = 5D
    val analyzed = Analyzed(1L, 1D, List(Measure(10L, price)))
    analyzed.minMaxSum should be(price, price, price)
  }

  "An Analyzed object with values" should "returns a tuple for minMaxSum with the calculated value" in {
    val minPrice = 5D
    val maxPrice = 10D
    val analyzed = Analyzed(1L, 1D, List(Measure(10L, minPrice), Measure(10L, maxPrice)))
    analyzed.minMaxSum should be(minPrice, maxPrice, minPrice + maxPrice)
  }

  "An Analyzed object with measures" should "returns a filtered list for recentMeasures" in {
    val analyzed = Analyzed(1L, 1D, List(Measure(1L, 0D), Measure(2L, 0D), Measure(3L, 0D), Measure(5L, 0D)))
    analyzed.recentMeasures(3) should be(List[Measure](Measure(5L, 0D)))
  }


  "An Analyzed object when printed" should "returns a formatted String" in {
    val analyzed = Analyzed(1355270646, 1.80195, List(Measure(1355270609, 1.80215), Measure(1355270621, 1.80185),
      Measure(1355270646, 1.80195)))
    analyzed.toString should be("1355270646 1.80195 3 5.40595 1.80185 1.80215")
  }

}
