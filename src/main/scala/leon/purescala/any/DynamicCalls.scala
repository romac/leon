/* Copyright 2009-2015 EPFL, Lausanne */

package leon
package purescala
package any

import Types._
import Definitions._
import Expressions._

class DynamicCalls(ctx: LeonContext, program: Program) {

  private val ops = program.library.anyOps

  private val opsMap = Map(
    "$plus"  -> ops.plus.get,
    "$minus" -> ops.minus.get,
    "$times" -> ops.times.get
  )

  private
  def applyOp(opDef: FunDef)(lhs: Expr, rhs: Expr): Expr = {
    val tfd = TypedFunDef(opDef, Seq())
    FunctionInvocation(tfd, Seq(lhs, rhs))
  }

  def desugar(opName: String, lhs: Expr, rhs: Expr): Option[Expr] = {
    opsMap.get(opName).map { opDef =>
      applyOp(opDef)(lhs, rhs)
    }
  }

}
