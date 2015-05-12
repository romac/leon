
import scala.language.dynamics

import leon.lang._
import leon.annotation._

object anyDynamic {

  @ignore
  def native: Nothing = throw new RuntimeException("native should never be called")

  @library
  abstract class AnyDynamic extends Dynamic {

    @ignore
    def applyDynamic(name: java.lang.String)(args: Any*): AnyDynamic = native

    @ignore
    def selectDynamic(name: java.lang.String): AnyDynamic = native

    @ignore
    def +(that: AnyDynamic): AnyDynamic = native
  }

  case class AnyInt(value: Int) extends AnyDynamic
  case class AnyBigInt(value: BigInt) extends AnyDynamic

  def add(x: AnyDynamic, y: AnyDynamic): AnyDynamic = {
    x doSomethingWith y
    x getThat

    x addWith y andWith (x getThat)
  }

}
