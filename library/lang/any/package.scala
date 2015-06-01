/* Copyright 2009-2015 EPFL, Lausanne */

package leon.lang

import leon.lang.string._
import leon.annotation._
import scala.language.implicitConversions

package object any {

  @library
  abstract class Any1

  @ignore
  def native[A]: A = throw new RuntimeException("native should never be called")

  @ignore
  implicit class Any1Ops(val value: Any) {

    def +(that: Any): Any1 = native
    def -(that: Any): Any1 = native

  }

  @library
  def plusOp(lhs: Any, rhs: Any): Any = (lhs, rhs) match {
    case (l: Int, r: Int)       => l + r
    case (l: BigInt, r: BigInt) => l + r
    case _                      => error[Any]("operation not supported")
  }

  @library
  def minusOp(lhs: Any, rhs: Any): Any = (lhs, rhs) match {
    case (l: Int, r: Int)       => l - r
    case (l: BigInt, r: BigInt) => l - r
    case _                      => error[Any]("operation not supported")
  }

}
