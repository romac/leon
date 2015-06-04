
import leon.lang._
import leon.lang.string._
import leon.lang.any._

object InsertionSort {

  sealed abstract class OptionA
  case class SomeA(value: Any) extends OptionA
  case class NoneA() extends OptionA

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
  }

  case class ConsA(h: Any, t: ListA) extends ListA
  case class NilA() extends ListA

  implicit class AnyListOps(val value: Any) {

    def head: Any = value match {
      case l: ListA => l.head
      case _        => NoneA()
    }

    def head2: Any = {
      require(value.isInstanceOf[ListA])
      value match {
        case l: ListA => l.head
      }
    }

    // def tail: Option

  }

}

