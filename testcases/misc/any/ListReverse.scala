
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

    def contents: Set[Any] = this match {
      case NilA()       => Set.empty[Any]
      case ConsA(x, xs) => Set(x) ++ xs.contents
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
      require(value.isInstanceOf[ListA] && value != NilA())
      value match { case l: ListA =>
        l.head
      }
    }

    def tail: ListA = {
      require(value.isInstanceOf[ListA] && value != NilA())
      value match { case l: ListA =>
        l.tail
      }
    }

    def size: BigInt = {
      require(value.isInstanceOf[ListA])
      value match { case l: ListA =>
        l.size
      }
    }

    def contents: Set[Any] = {
      require(value.isInstanceOf[ListA])
      value match { case l: ListA =>
        l.contents
      }
    }

    def ++(other: ListA): ListA = {
      require(value.isInstanceOf[ListA])
      value match { case l: ListA =>
        l ++ other
      }
    }

  }

  def reverse(l: Any) : Any = {
    require(l.isInstanceOf[ListA])
    reverse0(l, NilA())
  } ensuring(_.contents == l.contents)

  def reverse0(l1: Any, l2: Any) : Any = {
    require(l1.isInstanceOf[ListA] && l2.isInstanceOf[ListA])
    l1 match {
      case NilA() => l2
      case ConsA(x, xs) =>
        val list2 = l2 match { case x: ListA => x }
        reverse0(xs, ConsA(x, list2))
    }
  } ensuring(_.contents == l1.contents ++ l2.contents)

  def reverseReverseEqIdentity(lst: Any) = {
    require(lst.isInstanceOf[ListA])
    reverse(reverse(lst)) == lst
  }.holds

  def reverse2(lst: Any): Any = {
    require(lst.isInstanceOf[ListA])
    if (lst == NilA()) lst
    else reverse2(lst.tail) ++ ConsA(lst.head, NilA())
  } ensuring { _.contents == lst.contents }

  def reverse2Reverse2EqIdentity(lst: Any) = {
    require(lst.isInstanceOf[ListA])
    reverse2(reverse2(lst)) == lst
  }.holds

}
