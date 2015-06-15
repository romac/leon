
import leon.lang._
import leon.lang.string._
import leon.lang.any._

object ListReverse {

  sealed abstract class ListA {
    def head: Any = {
      require(this != NilA())
      val ConsA(h, _) = this
      h
    }

    def tail: ListA = {
      require(this != NilA())
      val ConsA(_, t) = this
      t
    }

    def size: BigInt = { this match {
      case NilA() => 0
      case ConsA(_, t) => 1 + t.size
    } } ensuring { _ >= 0 }

    def content: Set[Any] = this match {
      case NilA()       => Set.empty[Any]
      case ConsA(x, xs) => Set(x) ++ xs.content
    }

    def ++(other: ListA): ListA = { this match {
      case NilA()      => other
      case ConsA(h, t) => ConsA(h, t ++ other)
    } } ensuring { _.size == this.size + other.size }
  }

  case class ConsA(h: Any, t: ListA) extends ListA
  case class NilA() extends ListA

  implicit class AnyListOps(val value: Any) {

    def head: Any = {
      // require(value.isInstanceOf[ListA])
      value match { case l: ListA =>
        l.head
      }
    }

    def tail: ListA = {
      // require(value.isInstanceOf[ListA])
      value match { case l: ListA =>
        l.tail
      }
    }

    def size: BigInt = {
      // require(value.isInstanceOf[ListA])
      value match { case l: ListA =>
        l.size
      }
    }

    def content: Set[Any] = {
      // require(value.isInstanceOf[ListA])
      value match { case l: ListA =>
        l.content
      }
    }

    def ++(other: ListA): ListA = {
      // require(value.isInstanceOf[ListA])
      value match { case l: ListA =>
        l ++ other
      }
    }

  }

  def reverse(lst: Any): Any = {
    // require(lst.isInstanceOf[ListA])
    lst match {
      case NilA() => NilA()
      case _      => reverse(lst.tail) ++ ConsA(lst.head, NilA())
    }
  } ensuring { _.size == lst.size }

  def reverseReverseEqIdentity(lst: Any) = {
    // require(lst.isInstanceOf[ListA])
    reverse(reverse(lst)) == lst
  }.holds

}
