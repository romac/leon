
import leon.lang._
import leon.lang.any._
import leon.lang.string._

object AnyExpr {

  sealed abstract class Expr
  case class Lit(value: Any) extends Expr
  case class Add(lhs: Expr, rhs: Expr) extends Expr
  case class Error(msg: String) extends Expr

  def eval(expr: Expr): Any = expr match {
    case Lit(value) =>
      value

    case Add(lhs, rhs) =>
      (eval(lhs), eval(rhs)) match {
        case (li: Int, ri: Int) =>
          li + ri
        case (lb: BigInt, rb: BigInt) =>
          lb + rb
        case (ls: String, rs: String) =>
          ls + rs
        case _ =>
          error[Any]("Cannot add non-numeric values together")
      }

      case Error(msg) =>
        error[Any]("An error occurred")
  }

}
