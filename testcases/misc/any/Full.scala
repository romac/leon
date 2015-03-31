
import leon.lang._

object Full {

  case class Foo(value: Int)
  case class Bar(value: Boolean)

  val x: Any = Foo(42)
  val y: ?!? = Foo(42)

  def id(x: Any): Any = x
  def id2(x: ?!?): ?!? = x
  def id3(x: Any): ?!? = x
  def id4(x: ?!?): Any = x

  def fooOrBar(which: Boolean): Any =
    if (which) Foo(23) else Bar(which)

  def fooOrBar2(which: Boolean): Any = which match {
    case true => Foo(23)
    case false => Bar(which)
  }

  def fooOrBar3(which: Boolean): Any =
    fooOrBar2(which)

  val foo: Any = Foo(42)
  val foo2: Foo = Foo(42)

  def test1: Unit = {
    if (true) id(foo) else id(foo2)
  }

  def test2(x: Any): Int = x match {
    case y: Foo => y.value
    case Foo(y) => y
    case _      => -1
  }

  def test3(x: Any): Int =
    if (x.isInstanceOf[Foo]) x.asInstanceOf[Foo].value
    else -1

  def testRec(x: Any): Int = {
    def rec(y: Any): Any = {
      Foo(111)
    }
    rec(x).asInstanceOf[Foo].value
  }

  abstract class Expr
  case class Lit(value: Int) extends Expr
  case class Add(lhs: Expr, rhs: Expr) extends Expr

  def eval(e: Any): Int = e match {
    case Lit(v) => v
    case Add(l, r) => eval(l) + eval(r)
    case _ => -1
  }

  def isExpr(e1: Any): Boolean =
    e1.isInstanceOf[Expr]

  def isExpr2(e2: Any): Boolean = e2 match {
    case _: Expr => true
    case _ => false
  }

  def isExpr3(e3: Any): Boolean = e3 match {
    case _: Lit => true
    case _: Add => true
    case _ => false
  }

  def isExpr4(e4: Any): Boolean = e4.isInstanceOf[Expr]
  def isExpr5(e5: Any): Boolean = e5.isInstanceOf[Lit] || e5.isInstanceOf[Add]

  def getValue(x: Any): Int = x match {
    case Foo(value) => value
    case e: Expr => this.eval(e)
    case _ => -1
  }

  case class WithAnyMul(obj: Any) {
    def *(n: Int): Any = obj match {
      // case i: Int => i * n
      // case s: String => s + n
      case e: Expr => e match {
        case Lit(l) => Lit(l * n)
        case Add(l, r) => Add((WithAnyMul(l) * n).asInstanceOf[Expr], (WithAnyMul(r) * n).asInstanceOf[Expr])
      }
    }
  }

  val add12: Any = Add(Lit(1), Lit(2))

}
