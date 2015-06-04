/* Copyright 2009-2015 EPFL, Lausanne */

package leon.lang

import scala.language.implicitConversions

import leon.annotation._
import leon.lang.string._

package object any {

  @library
  abstract class Any1

  @ignore
  def native[A]: A = throw new RuntimeException("native should never be called")

  @library
  implicit class Any1Ops(val value: Any) {

    def +(other: Any): Any = plusOp(value, other)
    def -(other: Any): Any = minusOp(value, other)
    def *(other: Any): Any = timesOp(value, other)

  }

  @library
  def plusOp(lhs: Any, rhs: Any): Any = (lhs, rhs) match {
    case (l: Int, r: Int)       => l + r
    case (l: BigInt, r: BigInt) => l + r
    case (l: String, r: String) => l + r
    case _                      => error[Any]("operation not supported")
  }

  @library
  def minusOp(lhs: Any, rhs: Any): Any = (lhs, rhs) match {
    case (l: Int, r: Int)       => l - r
    case (l: BigInt, r: BigInt) => l - r
    case _                      => error[Any]("operation not supported")
  }

  @library
  def timesOp(lhs: Any, rhs: Any): Any = (lhs, rhs) match {
    case (l: Int, r: Int)       => l * r
    case (l: BigInt, r: BigInt) => l * r
    case (l: String, r: BigInt) => l repeat r
    case _                      => error[Any]("operation not supported")
  }

}
