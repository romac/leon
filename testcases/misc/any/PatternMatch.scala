
import leon.lang._

object PatternMatch {

  abstract class Expr
  case class Lit(value: Int) extends Expr
  case class Add(left: Expr, right: Expr) extends Expr

  def eval(e: Expr): Int = e match {
    case Lit(n)    => n
    case Add(l, r) => eval(l) + eval(r)
  }

  case class IntBox(value: Int)
  case class Rational(num: Int, denum: Int)
  case class Unknown(value: Any)
  case class Other()

  def value(x: Any): Int = x match {
    // case Any1$Expr(e: Expr) => eval(e)
    case e: Expr              => eval(e)

    // case Any1$IntBox(IntBox(n)) => n
    case IntBox(n)            => n

    // case Any1$Rational(Rational(num, denum)) => num / denum
    case Rational(num, denum) => num / denum

    // case Any1$Unknown(Unknown(Any1$Expr(e: Expr))) => eval(e)
    case Unknown(e: Expr)     => eval(e)

    // case Any1$Unknown(Unknown(y)) => value(y)
    case Unknown(y)           => value(y)

    // case Any1$Other(_: Other) => 42
    case _: Other             => 42

    // case o @ _             => -1
    case o @ _                => -1
  }

  def wrap(x: Any): Any = x match {
    case e: Expr => IntBox(eval(e))
    case _       => Unknown(x)
  }

  def recId(x: Any): Any = {
    def rec(x: Any, again: Boolean): Any = if (again) rec(x, false) else x
    rec(x, true)
  }

  def recId2(x: Expr): Any = {
    def rec2(x: Any, again: Boolean): Any = if (again) rec2(x, false) else x
    rec2(x, true)
  }

  def idThisMethod(x: Expr): Any = {
    this.recId(x)
  }

  case object idObj {
    def apply(x: Any): Any = x
  }

  case class IdClass() {
    def apply(x: Any): Any = x
  }

  // def idObjMethod(x: Expr): Expr = idObj.apply(x).asInstanceOf[Expr]
  // def idClassMethod(x: Expr): Expr = (new IdClass()).apply(x).asInstanceOf[Expr]
  def idObjMethod(x: Expr): Any = idObj.apply(x)
  def idClassMethod(x: Expr): Any = IdClass().apply(x)

  // val a: Any = Lit(1)
  // val b: Any = IntBox(2)

  // (a, b) match {
  //   case (Lit(n), IntBox(m)) if n == m => true
  //   case (IntBox(n), Lit(m)) if n == m => true
  //   case _                             => false
  // }

}
