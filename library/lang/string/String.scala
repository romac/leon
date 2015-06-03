/* Copyright 2009-2015 EPFL, Lausanne */

package leon.lang.string

import leon.annotation._

@library
case class String(chars: leon.collection.List[Char]) {
  def +(that: String): String = {
    String(this.chars ++ that.chars)
  }

  def size = chars.size

  def length = size

  def repeat(n: BigInt): String = {
    require(n >= 1)
    if (n == 1) this
    else String(chars ++ chars).repeat(n - 1)
  } ensuring { _.length == this.length * n }

}
