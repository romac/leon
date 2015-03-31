/* Copyright 2009-2015 EPFL, Lausanne */

package leon
package purescala

import Definitions.Program

object DesugarAnyPhase extends TransformationPhase {

  val name = "Desugar Any"
  val description = "Desugar Any into case classes extending Any1"

  def apply(ctx: LeonContext, p: Program): Program = {

    val phases =
      DeclareAnyWrappers     andThen
      ReplaceTypeAnyWithAny1 andThen
      WrapAnyExprs           andThen
      PrinterPhase

    phases.run(ctx)(p)
  }
}

object PrinterPhase extends TransformationPhase {
  val name = "Printer"
  val description = "Print the program"

  def apply(ctx: LeonContext, p: Program): Program = {
    ctx.reporter.info(PrettyPrinter(p))
    p
  }
}
