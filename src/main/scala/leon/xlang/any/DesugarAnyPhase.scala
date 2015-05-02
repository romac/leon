/* Copyright 2009-2015 EPFL, Lausanne */

package leon
package xlang
package any

import leon.purescala._

import Definitions.Program

object DesugarAnyPhase extends TransformationPhase {

  val name = "Desugar Any"
  val description = "Provide support for Any via case class wrappers"

  def apply(ctx: LeonContext, p: Program): Program = {

    val phases =
      DeclareAnyWrappers     andThen
      ReplaceTypeAnyWithAny1 andThen
      WrapAnyExprs           andThen
      PrinterPhase(name)

    phases.run(ctx)(p)
  }
}

case class PrinterPhase(header: String) extends TransformationPhase {
  val name = "Printer"
  val description = "Print the program"

  val opts =
    PrinterOptions(
      baseIndent     = 0,
      printPositions = false,
      printTypes     = true,
      printUniqueIds = true
    )

  def apply(ctx: LeonContext, p: Program): Program = {
    ctx.reporter.info(
      "#" * (header.length + 4) + "\n" +
      "# " + header + " #" + "\n" +
      "#" * (header.length + 4) + "\n" +
      ScalaPrinter(p, opts)
    )

    p
  }
}
