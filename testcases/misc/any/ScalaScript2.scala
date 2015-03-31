import leon.lang._

object ScalaScript {

  case class Value(v: Int)

  abstract class ValueList {
    def contains(value: Value): Boolean = this match {
      case Nil => false
      case Cons(head, _) if head == value => true
      case Cons(_, tail) => tail.contains(value)
    }
  }
  case class Cons(head: Value, tail: ValueList) extends ValueList
  case object Nil extends ValueList

  def contains(list: ?!?, element: ?!?): Boolean = (list, element) match {
    case (l: ValueList, e: Value) => l.contains(e)
    case (e: Value, l: ValueList) => contains(l, e)
    case _ => false
  }

}
