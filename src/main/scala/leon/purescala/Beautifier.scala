/* Copyright 2009-2015 EPFL, Lausanne */

package leon
package purescala

import Definitions._
import Expressions._
import any.Any1Ops

class Beautifier(ctx: LeonContext, program: Program) {

  private val Any1Ops = new Any1Ops(ctx, program)

  def apply(expr: Expr) = beautify(expr)

  def beautify(expr: Expr): Expr = {
    Any1Ops.unliftExpr(expr)
  }

}
