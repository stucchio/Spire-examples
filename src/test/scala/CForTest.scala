package com.chrisstucchio.spire_examples

import spire.implicits._

class MultiplyBenchmark(func: Array[Double] => Array[Double]) extends testing.Benchmark {
  var x: Array[Double] = null

  override def setUp() = {
    val isNull = (x == null)
    if (isNull) {
      x = new Array(8*1024*1024)
      cfor(0)(_ < x.size, _ + 1)(i => {
        x(i) = java.lang.Math.random()
      })
    } else {
      cfor(0)(_ < x.size, _ + 1)(i => {
        x(i) = x(i) / 2.0
      })
    }
  }

  def run = func(x)
}

class DotBenchmark(func: (Array[Double], Array[Double]) => Array[Double]) extends testing.Benchmark {
  var x: Array[Double] = null
  var y: Array[Double] = null

  override def setUp() = {
    if (x == null) {
      x = new Array(8*1024*1024)
      cfor(0)(_ < x.size, _ + 1)(i => {
        x(i) = java.lang.Math.random()
      })
    }
    if (y == null) {
      y = new Array(8*1024*1024)
      cfor(0)(_ < x.size, _ + 1)(i => {
        y(i) = java.lang.Math.random()
      })
    }
  }

  def run = func(x,y)
}

object CForTest {
  val CForTest = new MultiplyBenchmark(CFor.cforMultiply)
  val MapTest = new MultiplyBenchmark(CFor.mapMultiply)
  val WhileTest = new MultiplyBenchmark(CFor.whileMultiply)

  val CForDotTest = new DotBenchmark(CFor.cforDotProd)
  val ZipDotTest = new DotBenchmark(CFor.zipDotProd)
}
