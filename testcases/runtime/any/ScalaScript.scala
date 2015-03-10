import leon.lang._
// import leon.lang.string._

object ScalaScript {

  abstract class IntList {
    def contains(element: Int): Boolean = this match {
      case Nil => false
      case Cons(h, _) if h == element => true
      case Cons(_, t) => t.contains(element)
    }
  }
  case class Cons(head: Int, tail: IntList) extends IntList
  case object Nil extends IntList

  abstract class Any1
  case class AnyIntList(value: IntList) extends Any1
  case class AnyInt(value: Int) extends Any1

  def contains(list: Any1, element: Any1): Boolean = (list, element) match {
    case (AnyIntList(l), AnyInt(e)) => l.contains(e)
    case (AnyInt(e), AnyIntList(l)) => contains(element, list)
    case _ => false
  }

  // def contains(list: ?!?, element: ?!?): Boolean = (list, element) match {
  //   case (l: IntList, e: Int) => l.contains(e)
  //   case (e: Int, l: IntList) => contains(l, e)
  //   case _ =>
  //     sys.error("Type error: expected IntList, Int or Int, IntList")
  // }

}
