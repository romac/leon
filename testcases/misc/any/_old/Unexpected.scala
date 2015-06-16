
object UnexpectedTest {

  sealed abstract class Expr
  case class Lit(value: Int) extends Expr
  case class Add(lhs: Expr, rhs: Expr) extends Expr

  def testExpr(e: Expr): Int = e match {
    case Lit(value) => value
  }

  def testAny(a: Any): Int = a match {
    case Lit(value) => value
    case 42 => 42
  }

}
