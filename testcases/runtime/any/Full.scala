
import leon.lang._

object Full {

  case class Foo(value: Int)

  val x: Any = Foo(42)
  val y: ?!? = Foo(42)

  def id(x: Any): Any = x
  def id2(x: ?!?): ?!? = x
  def id3(x: Any): ?!? = x
  def id4(x: ?!?): Any = x

  val foo: Any = Foo(42)

  def test1: Unit = {
    id(foo)
  }

  def test2(x: Any): Int = x match {
    case y: Foo => y.value
    case Foo(y) => y
    case _      => -1
  }

  def test3(x: Any): Int =
    if (x.isInstanceOf[Foo]) x.asInstanceOf[Foo].value
    else -1

  abstract class Expr
  case class Lit(value: Int) extends Expr
  case class Add(lhs: Expr, rhs: Expr) extends Expr

  def eval(e: Any): Int = e match {
    case Lit(v) => v
    case Add(l, r) => eval(l) + eval(r)
    case _ => -1
  }

  def isExpr(e: Any): Boolean =
    e.isInstanceOf[Expr]

  def isExpr2(e: Any): Boolean = e match {
    case _: Expr => true
    case _ => false
  }

  def isExpr3(e: Any): Boolean = e match {
    case _: Lit => true
    case _: Add => true
    case _ => false
  }

  def isExpr4(e: Any): Boolean = e.isInstanceOf[Expr]
  def isExpr5(e: Any): Boolean = e.isInstanceOf[Lit] || e.isInstanceOf[Add]

  def getValue(x: Any): Int = x match {
    case Foo(value) => value
    case e: Expr => eval(e)
    case _ => -1
  }

}
