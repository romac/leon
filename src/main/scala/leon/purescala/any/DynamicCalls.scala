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
    "$plus"  -> ops.Plus.get,
    "$minus" -> ops.Minus.get
  )

  def op(opDef: CaseClassDef)(lhs: Expr, rhs: Expr): Expr = {
    val opType = classDefToClassType(opDef).asInstanceOf[CaseClassType]
    val method = opDef.methods.filter(_.id.name == "apply").head
    val rec    = CaseClass(opType, Seq(lhs, rhs))
    val tfd    = TypedFunDef(method, Seq())

    MethodInvocation(rec, opDef, tfd, Seq(lhs, rhs))
  }

  def desugar(dc: DynamicCall): Option[Expr] = {
    val DynamicCall(name, lhs, rhs) = dc
    opsMap.get(name).map { opDef =>
      op(opDef)(lhs, rhs).copiedFrom(dc)
    }
  }

}
