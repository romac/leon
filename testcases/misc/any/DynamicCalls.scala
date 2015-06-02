
import leon.lang._
import leon.lang.string._
import leon.lang.any._

import leon.annotation._
import scala.language.implicitConversions

object DynamicCalls {

  // val anInt: Any = 12
  // val anotherInt: Any = 20
  // val result = anInt + anotherInt

  // def add(a: Any, b: Any): Any = {
  //   a + b - b
  // } ensuring { res => res == a }

  def size(x: Any): BigInt = x match {
    case b: BigInt => b
    case s: String => s.size
    case _         => 0
  }

  val a: BigInt = 2
  val b: BigInt = 3

  def lemma1(a: Any, b: Any) = {
    size(a * b) == size(a) * size(b)
  }.holds

}
