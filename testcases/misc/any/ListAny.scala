/* Copyright 2009-2015 EPFL, Lausanne */
package leon.collection.any

import leon._
import leon.lang._
import leon.collection._
import leon.annotation._

sealed abstract class ListAny {

  def size: BigInt = (this match {
    case Nil() => BigInt(0)
    case Cons(h, t) => 1 + t.size
  }) ensuring (_ >= 0)

  def content: Set[Any] = this match {
    case Nil() => Set()
    case Cons(h, t) => Set(h) ++ t.content
  }

  def contains(v: Any): Boolean = (this match {
    case Cons(h, t) if h == v => true
    case Cons(_, t) => t.contains(v)
    case Nil() => false
  }) ensuring { res => res == (content contains v) }

  def ++(that: ListAny): ListAny = (this match {
    case Nil() => that
    case Cons(x, xs) => Cons(x, xs ++ that)
  }) ensuring { res =>
    (res.content == this.content ++ that.content) &&
    (res.size == this.size + that.size)
  }

  def head: Any = {
    require(this != Nil())
    val Cons(h, _) = this
    h
  }

  def tail: ListAny = {
    require(this != Nil())
    val Cons(_, t) = this
    t
  }

  def apply(index: BigInt): Any = {
    require(0 <= index && index < size)
    if (index == BigInt(0)) {
      head
    } else {
       tail(index-1)
    }
  }

  def ::(t: Any): ListAny = Cons(t, this)

  def :+(t: Any): ListAny = {
    this match {
      case Nil() => Cons(t, this)
      case Cons(x, xs) => Cons(x, xs :+ (t))
    }
  } ensuring(res => (res.size == size + 1) && (res.content == content ++ Set(t)))

  def reverse: ListAny = {
    this match {
      case Nil() => this
      case Cons(x,xs) => xs.reverse :+ x
    }
  } ensuring (res => (res.size == size) && (res.content == content))

  def take(i: BigInt): ListAny = { (this, i) match {
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

  def drop(i: BigInt): ListAny = { (this, i) match {
    case (Nil(), _) => Nil()
    case (Cons(h, t), i) =>
      if (i <= BigInt(0)) {
        Cons(h, t)
      } else {
        t.drop(i-1)
      }
  }} ensuring { _.size == (
    if      (i <= 0)         this.size
    else if (i >= this.size) BigInt(0)
    else                     this.size - i
  )}

  def slice(from: BigInt, to: BigInt): ListAny = {
    require(0 <= from && from <= to && to <= size)
    drop(from).take(to-from)
  }

  def replace(from: Any, to: Any): ListAny = { this match {
    case Nil() => Nil()
    case Cons(h, t) =>
      val r = t.replace(from, to)
      if (h == from) {
        Cons(to, r)
      } else {
        Cons(h, r)
      }
  }} ensuring { res =>
    res.size == this.size &&
    res.content == (
      (this.content -- Set(from)) ++
      (if (this.content contains from) Set(to) else Set[Any]())
    )
  }

  private def chunk0(s: BigInt, l: ListAny, acc: ListAny, res: ListAny, s0: BigInt): ListAny = l match {
    case Nil() =>
      if (acc.size > 0) {
        res :+ acc
      } else {
        res
      }
    case Cons(h, t) =>
      if (s0 == BigInt(0)) {
        chunk0(s, l, Nil(), res :+ acc, s)
      } else {
        chunk0(s, t, acc :+ h, res, s0-1)
      }
  }

  def chunks(s: BigInt): ListAny = {
    require(s > 0)

    chunk0(s, this, Nil(), Nil(), s)
  }

  def zip(that: ListAny): ListAny = { (this, that) match {
    case (Cons(h1, t1), Cons(h2, t2)) =>
      Cons((h1, h2), t1.zip(t2))
    case (_) =>
      Nil()
  }} ensuring { _.size == (
    if (this.size <= that.size) this.size else that.size
  )}

  def -(e: Any): ListAny = { this match {
    case Cons(h, t) =>
      if (e == h) {
        t - e
      } else {
        Cons(h, t - e)
      }
    case Nil() =>
      Nil()
  }} ensuring { _.content == this.content -- Set(e) }

  def --(that: ListAny): ListAny = { this match {
    case Cons(h, t) =>
      if (that.contains(h)) {
        t -- that
      } else {
        Cons(h, t -- that)
      }
    case Nil() =>
      Nil()
  }} ensuring { _.content == this.content -- that.content }

  def &(that: ListAny): ListAny = { this match {
    case Cons(h, t) =>
      if (that.contains(h)) {
        Cons(h, t & that)
      } else {
        t & that
      }
    case Nil() =>
      Nil()
  }} ensuring { _.content == (this.content & that.content) }

  def pad(s: BigInt, e: Any): ListAny = (this, s) match {
    case (_, s) if s <= 0 =>
      this
    case (Nil(), s) =>
      Cons(e, Nil().pad(s-1, e))
    case (Cons(h, t), s) =>
      Cons(h, t.pad(s-1, e))
  }

  def find(e: Any): Option[BigInt] = { this match {
    case Nil() => None()
    case Cons(h, t) =>
      if (h == e) {
        Some(0)
      } else {
        t.find(e) match {
          case None()  => None()
          case Some(i) => Some(i+1)
        }
      }
  }} ensuring { _.isDefined == this.contains(e) }

  def init: ListAny = (this match {
    case Cons(h, Nil()) =>
      Nil()
    case Cons(h, t) =>
      Cons(h, t.init)
    case Nil() =>
      Nil()
  }) ensuring ( (r: ListAny) => ((r.size < this.size) || (this.size == BigInt(0))) )

  def last: Any = {
    require(!isEmpty)
    this match {
      case Cons(h, Nil()) => h
      case Cons(_, t) => t.last
    }
  }

  def lastOption: Option[Any] = this match {
    case Cons(h, t) =>
      t.lastOption.orElse(Some(h))
    case Nil() =>
      None()
  }

  def firstOption: Option[Any] = this match {
    case Cons(h, t) =>
      Some(h)
    case Nil() =>
      None()
  }

  def unique: ListAny = this match {
    case Nil() => Nil()
    case Cons(h, t) =>
      Cons(h, t.unique - h)
  }

  def splitAt(e: Any): ListAny = split(Cons(e, Nil()))

  def split(seps: ListAny): ListAny = this match {
    case Cons(h, t) =>
      if (seps.contains(h)) {
        Cons(Nil(), t.split(seps))
      } else {
        val Cons(rh: ListAny, rt) = t.split(seps)
        Cons(Cons(h, rh), rt)
      }
    case Nil() =>
      Cons(Nil(), Nil())
  }

  def count(e: Any): BigInt = this match {
    case Cons(h, t) =>
      if (h == e) {
        1 + t.count(e)
      } else {
        t.count(e)
      }
    case Nil() =>
      BigInt(0)
  }

  def evenSplit: (ListAny, ListAny) = {
    val c = size/2
    (take(c), drop(c))
  }

  def insertAt(pos: BigInt, l: ListAny): ListAny = {
    if(pos < 0) {
      insertAt(size + pos, l)
    } else if(pos == BigInt(0)) {
      l ++ this
    } else {
      this match {
        case Cons(h, t) =>
          Cons(h, t.insertAt(pos-1, l))
        case Nil() =>
          l
      }
    }
  }

  def replaceAt(pos: BigInt, l: ListAny): ListAny = {
    if(pos < 0) {
      replaceAt(size + pos, l)
    } else if(pos == BigInt(0)) {
      l ++ this.drop(l.size)
    } else {
      this match {
        case Cons(h, t) =>
          Cons(h, t.replaceAt(pos-1, l))
        case Nil() =>
          l
      }
    }
  }

  def rotate(s: BigInt): ListAny = {
    if (s < 0) {
      rotate(size+s)
    } else {
      val s2 = s % size
      drop(s2) ++ take(s2)
    }
  }

  def isEmpty = this match {
    case Nil() => true
    case _ => false
  }

  // Higher-order API
  def map(f: Any => Any): ListAny = { this match {
    case Nil() => Nil()
    case Cons(h, t) => f(h) :: t.map(f)
  }} ensuring { _.size == this.size}

  def foldLeft[R](z: R)(f: (R, Any) => R): R = this match {
    case Nil() => z
    case Cons(h,t) => t.foldLeft(f(z,h))(f)
  }

  def foldRight[R](f: (Any, R) => R)(z: R): R = this match {
    case Nil() => z
    case Cons(h, t) => f(h, t.foldRight(f)(z))
  }

  def scanLeft(z: Any)(f: (Any, Any) => Any): ListAny = this match {
    case Nil() => z :: Nil()
    case Cons(h,t) => z :: t.scanLeft(f(z,h))(f)
  }

  def scanRight(f: (Any, Any) => Any)(z: Any): ListAny = { this match {
    case Nil() => z :: Nil()
    case Cons(h, t) =>
      val rest@Cons(h1,_) = t.scanRight(f)(z)
      f(h, h1) :: rest
  }} ensuring { !_.isEmpty }

  def flatMap(f: Any => ListAny): ListAny =
    ListAnyOps.flatten(this map f)

  def filter(p: Any => Boolean): ListAny = { this match {
    case Nil() => Nil()
    case Cons(h, t) if p(h) => Cons(h, t.filter(p))
    case Cons(_, t) => t.filter(p)
  }} ensuring { res => res.size <= this.size && res.forall(p) }

  // In case we implement for-comprehensions
  def withFilter(p: Any => Boolean) = filter(p)

  def forall(p: Any => Boolean): Boolean = this match {
    case Nil() => true
    case Cons(h, t) => p(h) && t.forall(p)
  }

  def exists(p: Any => Boolean) = !forall(!p(_))

  def find(p: Any => Boolean): Option[Any] = { this match {
    case Nil() => None()
    case Cons(h, t) if p(h) => Some(h)
    case Cons(_, t) => t.find(p)
  }} ensuring { _.isDefined == exists(p) }

  def groupBy(f: Any => Any): Map[Any, ListAny] = this match {
    case Nil() => Map.empty[Any, ListAny]
    case Cons(h, t) =>
      val key: Any = f(h)
      val rest: Map[Any, ListAny] = t.groupBy(f)
      val prev: ListAny = if (rest isDefinedAt key) rest(key) else Nil()
      (rest ++ Map((key, h :: prev))) : Map[Any, ListAny]
  }

  def takeWhile(p: Any => Boolean): ListAny = { this match {
    case Cons(h,t) if p(h) => Cons(h, t.takeWhile(p))
    case _ => Nil()
  }} ensuring { _ forall p }
}

@ignore
object ListAny {
  def apply(elems: Any*): ListAny = ???
}

@library
object ListAnyOps {
  def flatten(ls: ListAny): ListAny = ls match {
    case Cons(h: ListAny, t) => h ++ flatten(t)
    case Nil() => Nil()
  }

  def isSorted(ls: ListAny): Boolean = ls match {
    case Nil() => true
    case Cons(_: BigInt, Nil()) => true
    case Cons(h1: BigInt, Cons(h2: BigInt, _)) if(h1 > h2) => false
    case Cons(_: BigInt, t) => isSorted(t)
  }

  def sorted(ls: ListAny): ListAny = ls match {
    case Cons(h: BigInt, t) => insSort(sorted(t), h)
    case Nil() => Nil()
  }

  def insSort(ls: ListAny, v: BigInt): ListAny = ls match {
    case Nil() => Cons(v, Nil())
    case Cons(h: BigInt, t) =>
      if (v <= h) {
        Cons(v, t)
      } else {
        Cons(h, insSort(t, v))
      }
  }
}

case class Cons(h: Any, t: ListAny) extends ListAny
case class Nil() extends ListAny

// @library
object ListSpecs {

  def snocIndex(l : ListAny, t : Any, i : BigInt) : Boolean = {
   require(0 <= i && i < l.size + 1)
   // proof:
   (l match {
     case Nil() => true
     case Cons(x, xs) => if (i > 0) snocIndex(xs, t, i-1) else true
   }) &&
   // claim:
   ((l :+ t).apply(i) == (if (i < l.size) l(i) else t))
  }.holds

  def reverseIndex(l : ListAny, i : BigInt) : Boolean = {
   require(0 <= i && i < l.size)
   (l match {
     case Nil() => true
     case Cons(x,xs) => snocIndex(l, x, i) && reverseIndex(l,i)
   }) &&
   (l.reverse.apply(i) == l.apply(l.size - 1 - i))
  }.holds

  def appendIndex(l1 : ListAny, l2 : ListAny, i : BigInt) : Boolean = {
   require(0 <= i && i < l1.size + l2.size)
   (l1 match {
     case Nil() => true
     case Cons(x,xs) => if (i==BigInt(0)) true else appendIndex(xs,l2,i-1)
   }) &&
   ((l1 ++ l2).apply(i) == (if (i < l1.size) l1(i) else l2(i - l1.size)))
  }.holds

  def appendAssoc(l1 : ListAny, l2 : ListAny, l3 : ListAny) : Boolean = {
   (l1 match {
     case Nil() => true
     case Cons(x,xs) => appendAssoc(xs,l2,l3)
   }) &&
   (((l1 ++ l2) ++ l3) == (l1 ++ (l2 ++ l3)))
  }.holds

  def snocIsAppend(l : ListAny, t : Any) : Boolean = {
   (l match {
     case Nil() => true
     case Cons(x,xs) =>  snocIsAppend(xs,t)
   }) &&
   ((l :+ t) == l ++ Cons(t, Nil()))
  }.holds

  def snocAfterAppend(l1 : ListAny, l2 : ListAny, t : Any) : Boolean = {
   (l1 match {
     case Nil() => true
     case Cons(x,xs) =>  snocAfterAppend(xs,l2,t)
   }) &&
   ((l1 ++ l2) :+ t == (l1 ++ (l2 :+ t)))
  }.holds

  def snocReverse(l : ListAny, t : Any) : Boolean = {
   (l match {
     case Nil() => true
     case Cons(x,xs) => snocReverse(xs,t)
   }) &&
   ((l :+ t).reverse == Cons(t, l.reverse))
  }.holds

  def reverseReverse(l : ListAny) : Boolean = {
   (l match {
     case Nil() => true
     case Cons(x,xs) => reverseReverse(xs) && snocReverse(xs.reverse, x)
   }) &&
   (l.reverse.reverse == l)
  }.holds

  // my hand calculation shows this should work, but it does not seem to be found
  // def reverseAppend(l1 : ListAny, l2 : ListAny) : Boolean = {
  //   (l1 match {
  //     case Nil() => true
  //     case Cons(x,xs) => {
  //       reverseAppend(xs,l2) &&
  //       snocAfterAppend(l2.reverse, xs.reverse, x) &&
  //       l1.reverse == (xs.reverse :+ x)
  //     }
  //   }) &&
  //   ((l1 ++ l2).reverse == (l2.reverse ++ l1.reverse))
  // }.holds

  // @induct
  // def folds(l : ListAny, z : Any, f : (Any, Any) => Any) = {
  //   { l match {
  //     case Nil() => true
  //     case Cons(h,t) => snocReverse(t, h)
  //   }} &&
  //   l.foldLeft(z)(f) == l.reverse.foldRight((x:Any,y:R) => f(y,x))(z)
  // }.holds

  // Can't prove this
  // @induct
  // def scanVsFoldLeft[A,B](l : List[A], z: B, f: (B,A) => B): Boolean = {
  //   l.scanLeft(z)(f).last == l.foldLeft(z)(f)
  // }.holds

  // @induct
  // def scanVsFoldRight[A,B](l: List[A], z: B, f: (A,B) => B): Boolean = {
  //   l.scanRight(f)(z).head == l.foldRight(f)(z)
  // }.holds

}
