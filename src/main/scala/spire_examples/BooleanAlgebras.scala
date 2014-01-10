package com.chrisstucchio.spire_examples

import scalaz._
import spire.algebra.BooleanAlgebra
import spire.syntax.booleanAlgebra._

object BooleanAlgebraExamples {

  implicit def PredicateBooleanAlgebra[T] = new BooleanAlgebra[T => Boolean] {
    def one: T => Boolean = _ => true
    def zero: T => Boolean = _ => false
    def and(a: T=>Boolean, b: T=>Boolean): T=>Boolean = x => a(x) && b(x)
    def or(a: T=>Boolean, b: T=>Boolean): T=>Boolean = x => a(x) || b(x)
    def complement(a: T=>Boolean) = x => !a(x)
  }

  def f(x: Int) = x % 2 == 0
  def g(x: Int) = x % 7 == 0
  val h = (f _) & (g _)
//  val f: Int => Boolean = ((x:Int) => x % 2 == 0) & ((x:Int) => x % 3 == 0)
  val x = 5 & 23

  trait BadFiniteSet[F] extends Enum[F] {
    val realMin: F
    override def min = Some(realMin)
    val realMax: F
    override def max = Some(realMax)
    def all: Set[F] = fromToL(realMin, realMax).toSet
  }

  implicit object FiveEnum extends BadFiniteSet[Int] {
    val realMin: Int = 1
    val realMax: Int = 5
    def succ(x: Int) = if (x < 5) { x + 1 } else { 5 }
    def pred(x: Int) = if (x > 1) { x - 1 } else { 1 }
    def order(x: Int, y: Int) = ???
  }

  implicit def FiniteSetBooleanAlgebra[T](implicit enum: BadFiniteSet[T]) = new BooleanAlgebra[Set[T]] {
    def one = enum.all
    def zero = Set[T]()
    def and(a: Set[T], b: Set[T]) = a intersect b
    def or(a: Set[T], b: Set[T]) = a union b
    def complement(a: Set[T]) = enum.all -- a
  }

  val r = Set(1,3,4) & Set(1,2,3)

  sealed trait FiniteOrCofinite[T]
  case class FiniteSet[T](x: Set[T]) extends FiniteOrCofinite[T]
  case class CoFiniteSet[T](x: Set[T]) extends FiniteOrCofinite[T] //represents everything not in x

  implicit def FiniteCoFiniteSetBooleanAlgebra[T] = new BooleanAlgebra[FiniteOrCofinite[T]] {
    def one = CoFiniteSet[T](Set[T]())
    def zero = FiniteSet(Set[T]())
    def and(a: FiniteOrCofinite[T], b: FiniteOrCofinite[T]) = (a,b) match {
      case (FiniteSet(x), FiniteSet(y)) => FiniteSet(x intersect y)
      case (CoFiniteSet(x), CoFiniteSet(y)) => CoFiniteSet(x union y)
      case (FiniteSet(x), CoFiniteSet(y)) => FiniteSet(x -- y)
      case (CoFiniteSet(x), FiniteSet(y)) => FiniteSet(y -- x)
    }
    def or(a: FiniteOrCofinite[T], b: FiniteOrCofinite[T]) = (a,b) match {
      case (FiniteSet(x), FiniteSet(y)) => FiniteSet(x union y)
      case (CoFiniteSet(x), CoFiniteSet(y)) => CoFiniteSet(x intersect y)
      case (FiniteSet(x), CoFiniteSet(y)) => CoFiniteSet(y -- x)
      case (CoFiniteSet(x), FiniteSet(y)) => CoFiniteSet(x -- y)
    }
    def complement(a: FiniteOrCofinite[T]) = a match {
      case FiniteSet(x) => CoFiniteSet(x)
      case CoFiniteSet(x) => FiniteSet(x)
    }
  }
}
