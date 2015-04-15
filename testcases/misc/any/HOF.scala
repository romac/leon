
object HOF {

  sealed abstract class Maybe {
    def map(f: Any => Any): Maybe = this match {
      case MJust(x) => MJust(f(x))
      case MNothing() => this
    }

    def flatMap(f: Any => Maybe): Maybe = this match {
      case MJust(x) => f(x)
      case MNothing() => this
    }
  }

  case class MJust(x: Any) extends Maybe
  case class MNothing() extends Maybe

  sealed abstract class Box
  case class BigIntBox(value: BigInt) extends Box
  case class AnyBox(value: Any) extends Box

  def mapFromAny(f1: Any => BigInt)(x: Any): BigInt =
    f1(x)

  def mapToAny(f2: BigInt => Any)(x: BigInt): Any =
    f2(x)

  def mapAny(f3: Any => Any)(x: Any): Any =
    f3(x)

  val bigInt: Any = BigInt(12)
  val maybeAny: Any = MJust(BigIntBox(BigInt(12)))

  val test1 = mapFromAny {
    case BigIntBox(value) => value
    case AnyBox(_) => BigInt(42)
  }(BigIntBox(50))

  val test3 = mapToAny(x => BigIntBox(x))(BigInt(23))

  val test4 = mapAny {
    case BigIntBox(value) => BigIntBox(value * 100)
    case AnyBox(value) => value
  }(AnyBox(BigIntBox(123)))

  val maybe = MJust(BigInt(42))

  val doubled = maybe.map { case BigIntBox(x) => BigIntBox(x * 2) }

  val tripled = maybe.flatMap { case BigIntBox(x) => MJust(BigIntBox(x * 2)) }

  case class PartialMatchTest(x: Any) {
    def isJust: Boolean = x match {
      case MJust(_) => true
      case MNothing() => false
    }
  }

}
