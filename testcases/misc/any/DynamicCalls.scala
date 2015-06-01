
import leon.lang._
import leon.lang.any._

import leon.annotation._
import scala.language.implicitConversions

object DynamicCalls {

  val anInt: Any = 12
  val anotherInt: Any = 20

  val result = anInt + anotherInt

  def add(a: Any, b: Any): Any = {
    a + b - b
  } ensuring { res => res == a }

}
