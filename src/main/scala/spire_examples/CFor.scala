package com.chrisstucchio.spire_examples

import spire.algebra.Field
import spire.algebra.VectorSpace
import spire.implicits._


object CFor {
  def cforMultiply(x: Array[Double]): Array[Double] = {
    val result = new Array[Double](x.size)
    cfor(0)(_ < x.size, _ + 1)(i => {
      result(i) = 2.0*x(i) + 3.0
    })
    result
  }

  def mapMultiply(x: Array[Double]): Array[Double] = x.map(x => 2.0*x+3.0)

  def whileMultiply(x: Array[Double]): Array[Double] = {
    val result = new Array[Double](x.size)
    var i:Int = 0
    while (i < x.size) {
      result(i) = 2.0*x(i) + 3.0
      i += 1
    }
    result
  }

  def cforDotProd(x: Array[Double], y: Array[Double]): Array[Double] = {
    val result = new Array[Double](x.size)
    cfor(0)(_ < x.size, _ + 1)(i => {
      result(i) = x(i)*y(i)
    })
    result
  }

  def zipDotProd(x: Array[Double], y: Array[Double]): Array[Double] = x.zip(y).map( x => x._1 * x._2 )
}
