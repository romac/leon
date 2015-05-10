/* Copyright 2009-2015 EPFL, Lausanne */

package leon
package purescala

import Expressions._

import any.Any1Ops.unwrapExpr

object Beautifier {

  private val beautifiers: Seq[Expr => Expr] = Seq(
    unwrapExpr
  )

  val beautify: Expr => Expr = beautifiers.reduce(_ andThen _)

}
