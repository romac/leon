/* Copyright 2009-2015 EPFL, Lausanne */

package leon.lang.any

import leon.annotation._

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
