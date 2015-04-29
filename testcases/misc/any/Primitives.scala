
import leon.lang._

object primitives {

  val anInt: Any = 42

  def doStuff(x: Any): Any = x match {
    case 12 => 13
    case i: Int => i + 1
    case c: Char => c
    case b: Boolean => !b
    // case (x: Int, y: Int) => (x + 1) + (y + 1)
    // case _ => 0
  }

  def swap(t: (Any, Any)): (Any, Any) = t match {
    case (x, y) => (y, x)
  }

  def id(x: Any): Any = x

  val anAny = id(anInt)
  val anotherInt = id(42)
  val aTuple = id((1, 2))
  val aBool = id(true)
  val aBigInt = id(BigInt(123))
  val aChar = id('z')
  val aSet = id(Set[Int](42))
  val aMap = id(Map[Int, BigInt](42-> BigInt(42)))
  val anArray = id(Array[Int](42))
  val anySet = id(Set[Any](anAny))

}
