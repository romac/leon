/* Copyright 2009-2015 EPFL, Lausanne */

package leon.lang

import leon.lang.string._
import leon.annotation._
import scala.language.implicitConversions

package object any {

  @ignore
  def native[A]: A = throw new RuntimeException("native should never be called")

  @ignore
  implicit class Any1Ops(val value: Any) {

    def +(that: Any): Any = native
    def -(that: Any): Any = native

  }

  @library
  case class Plus(lhs: Any, rhs: Any) {
    def apply(): Any = (lhs, rhs) match {
      case (l: Int, r: Int)       => l + r
      case (l: BigInt, r: BigInt) => l + r
    }
  }

  @library
  case class Minus(lhs: Any, rhs: Any) {
    def apply(): Any = (lhs, rhs) match {
      case (l: Int, r: Int)       => l - r
      case (l: BigInt, r: BigInt) => l - r
    }
  }

}
