
import leon.lang.Set

object DebugTypes {

  sealed abstract class List {

    def content: Set[Any] = this match {
      case Nil() => Set()
      case Cons(h, t) => Set(h) ++ t.content
    }

    def contains(v: Any): Boolean = (this match {
      case Cons(h, t) if h == v => true
      case Cons(_, t) => t.contains(v)
      case Nil() => false
    }) ensuring { res => res == (content contains v) }

    def take(i: BigInt): List = { (this, i) match {
      case (Nil(), _) => Nil()
      case (Cons(h, t), i) =>
        if (i <= BigInt(0)) {
          Nil()
        } else {
          Cons(h, t.take(i-1))
        }
    }} ensuring { _.size == (
      if      (i <= 0)         BigInt(0)
      else if (i >= this.size) this.size
      else                     i
    )}

    def size: BigInt = (this match {
      case Nil() => BigInt(0)
      case Cons(h, t) => 1 + t.size
    }) ensuring (_ >= 0)

  }

  case class Cons(h: Any, tail: List) extends List
  case class Nil() extends List

}
