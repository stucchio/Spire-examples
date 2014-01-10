package com.chrisstucchio.spire_examples

import spire.algebra.Field
import spire.algebra.VectorSpace
import spire.implicits._


object VectorSpaceExamples {
  implicit def FunctionSpace[T]  = new VectorSpace[T => Double, Double] {
    def scalar: Field[Double] = implicitly[Field[Double]]

    def zero: T => Double = (_ => 0.0)
    def negate(f: T => Double): T => Double = (x => -1*f(x))
    def plus(f: T => Double, g: T => Double): T => Double = (x => f(x)+g(x))
    def timesl(r: Double, f: T => Double): T => Double = (x => r*f(x))
  }

  implicit def FunctionToVectorSpaceSpace[U,V](implicit rng: VectorSpace[V,Double]) = new VectorSpace[U => V, Double] {
    type O = (U => V)

    def scalar: Field[Double] = implicitly[Field[Double]]
    def zero = _ => rng.zero
    def negate(f: O): O = x => -1.0 *: f(x)
    def plus(f: O, g: O): O = x => f(x) + g(x)
    def timesl(r: Double, f: O): O = x => r *: f(x)
  }

  def op(t: String => Double): (String => Double) = (x:String) => (2*t(x)+t(x)*t(x))

  val opx2: (String => Double) => (String => Double)  = 2.0 *: (op _)


  def MoreEfficientFunctionSpace[T]  = new VectorSpace[T => Double, Double] {
    private case class FunctionWithCoeffs(data: Map[T => Double, Double]) extends (T => Double) {
      def apply(t: T): Double = {
        var result: Double = 0.0
        data.foreach( fv => { result += fv._2 * fv._1(t); } )
        result
      }

      def addTo(other: FunctionWithCoeffs): FunctionWithCoeffs = {
        val intersect = data.keySet.intersect(other.data.keySet)
        val nonIntersecting = (data.keySet diff other.data.keySet) | (other.data.keySet diff data.keySet)
        val intersectingKeys = intersect.map(k => (k, data(k) + other.data(k))).toMap
        val nonIntersectingKeys = (data.keySet diff other.data.keySet).map(k => (k, data(k))).toMap + (other.data.keySet diff data.keySet).map(k => (k, other.data(k))).toMap
        FunctionWithCoeffs(intersectingKeys ++ nonIntersectingKeys)
      }

      def times(x: Double) = FunctionWithCoeffs(data.mapValues(v => x*v))
    }

    def scalar: Field[Double] = implicitly[Field[Double]]

    val zero: T => Double = (_ => 0.0)
    def negate(f: T => Double): T => Double = f match {
      case (fc:FunctionWithCoeffs) => fc.times(-1.0)
      case fo => FunctionWithCoeffs(Map(f -> -1))
    }
    def plus(f: T => Double, g: T => Double): T => Double = (f,g) match {
      case (fc:FunctionWithCoeffs, gc:FunctionWithCoeffs) => (fc addTo gc)
      case (fc:FunctionWithCoeffs, gc:(T=>Double)) => (fc addTo FunctionWithCoeffs(Map(gc -> 1.0)))
      case (fc:(T=>Double), gc:FunctionWithCoeffs) => (gc addTo FunctionWithCoeffs(Map(fc -> 1.0)))
      case (fc:(T=>Double), gc:(T => Double)) => FunctionWithCoeffs(Map(fc -> 1.0, gc -> 1.0))
    }
    def timesl(r: Double, f: T => Double): T => Double = f match {
      case (fc:FunctionWithCoeffs) => fc.times(r)
      case fo => FunctionWithCoeffs(Map(f -> r))
    }
  }

  def f(x: String) = x.size.toDouble
  def g(x: String) = x.hashCode.toDouble

  val h: String => Double = 4.0 *: (f _) + (g _)

  val t = 4.0 *: Vector(1.0,5.0,3.0) + Vector(2.0,1.0,-5.0)

  //Not implicit to avoid conflicting with Spire's version
  def SparseVectorsOver[T]  = new VectorSpace[Map[T,Double], Double] {
    def scalar: Field[Double] = implicitly[Field[Double]]

    def zero = Map()
    def negate(f: Map[T,Double]) = f.mapValues( -1 * _ )
    def plus(f: Map[T,Double], g: Map[T,Double]) = {
      val intersect = f.keySet.intersect(g.keySet)
      val intersectingKeys = intersect.map(k => (k, f(k) + g(k))).toMap
      val nonIntersectingKeys = (f.keySet diff g.keySet).map(k => (k, f(k))).toMap ++ (g.keySet diff f.keySet).map(k => (k, g(k))).toMap
      intersectingKeys ++ nonIntersectingKeys
    }
    def timesl(r: Double, f: Map[T,Double]) = f.mapValues( r * _)
  }
  val x = 3.0 *: Map("x" -> 5.0, "y" -> 2.5) + Map("y" -> -1.0, "z" -> 3.0)

}
