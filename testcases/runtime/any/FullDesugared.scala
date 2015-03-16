

import leon.lang._

object Full {

  case class Foo(fooVal: Int)
  case class Bar(barVal: Boolean)

  abstract class Any1
  case class Any1$Foo(value: Foo) extends Any1
  case class Any1$Bar(value: Bar) extends Any1

  abstract class Any1[T] {
    def value:
  }

  val x: Any1 = Any1$Foo(Foo(42))
  val y: Any1 = Any1$Foo(Foo(42))

  def id(x: Any1): Any1 = x
  def id2(x: Any1): Any1 = x
  def id3(x: Any1): Any1 = x
  def id4(x: Any1): Any1 = x

  def fooOrBar(which: Boolean): Any1 =
    if (which) Any1$Foo(Foo(23)) else Any1$Bar(Bar(which))

  def fooOrBar2(which: Boolean): Any1 = which match {
    case true => Any1$Foo(Foo(23))
    case false => Any1$(Bar(which))
  }

  val foo: Any1 = Any1$Foo(Foo(42))
  val foo2: Foo = Foo(42)

  def test1: Unit = {
    id(foo)
    id(Any1$Foo(foo2))
  }

  def test2(x: Any1): Int = x match {
    case y: Any1$Foo => y.value.fooVal
    case Any1$Foo(Foo(y)) => y
    case _      => -1
  }

  def test3(x: Any1): Int =
    if (x.isInstanceOf[Any1$Foo])
      x.asInstanceOf[Any$1Foo].value.fooVal
    else -1

  abstract class Expr
  case class Lit(value: Int) extends Expr
  case class Add(lhs: Expr, rhs: Expr) extends Expr

  case class Any1$Expr(value: Expr) extends Any1
  case class Any1$Lit(value: Lit) extends Any1 // <- Any1$Expr
  case class Any1$Add(value: Add) extends Any1 // Any1$Expr

  def eval(e: Any1): Int = e match {
    case Any1$Lit(Lit(v)) => v
    case Any1$Lit(Add(l, r)) => eval(l) + eval(r) // <- ???
    case _ => -1
  }

  def isExpr(e: Any1): Boolean =
    e.isInstanceOf[Any1$Expr]

  def isExpr2(e: Any1): Boolean = e match {
    case _: Any$1Expr => true
    case _ => false
  }

  def isExpr3(e: Any1): Boolean = e match {
    case _: Any1$Lit => true
    case _: Any1$Add => true
    case _ => false
  }

  def isExpr4(e: Any1): Boolean = e.isInstanceOf[Any1$Expr]
  def isExpr5(e: Any1): Boolean = e.isInstanceOf[Any1$Lit] || e.isInstanceOf[Any1$Add]

  def getValue(x: Any1): Int = x match {
    case Any$1Foo(Foo(value)) => value
    case e: Any1$Expr => eval(e)
    case _ => -1
  }

 case class WithAnyMul(obj: Any1) {
    def *(n: Int): Any1 = obj match {
      case i: Any1$Int => Any1$Int(i.value * n)
      case s: Any1$String => Any1$String(s.value + n)
      case e: Any1$Expr => e.value match {
        case Lit(l) => Any1$Lit(Lit(l * n))
        case Add(l, r) => Any1$Add(Add((l * n).asInstanceOf[Any1$Expr].value, (r * n).asInstanceOf[Any1$Expr].value))
      }
    }
  }

  val add12 = Add(Lit(1), Lit(2))
  val res = WithAnyMul(add12) * 3

}
