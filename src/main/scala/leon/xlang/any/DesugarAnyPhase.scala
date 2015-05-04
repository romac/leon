/* Copyright 2009-2015 EPFL, Lausanne */

package leon
package xlang
package any

import leon.purescala._

import Definitions.{Program, UnitDef}

object DesugarAnyPhase extends TransformationPhase {

  val name = "Desugar Any"
  val description = "Provide support for Any via case class wrappers"

  def apply(ctx: LeonContext, p: Program): Program = {

    val phases =
      ReplaceTypeAnyWithAny1 andThen
      WrapAnyExprs           andThen
      AddModuleAnyToUnit     andThen
      PrinterPhase(name)

    phases.run(ctx)(p)
  }

  object AddModuleAnyToUnit extends TransformationPhase {
    val name = "Add Module Any"
    val description = "Add module Any to program's unit"

    // FIXME: Should add a new unit for Any, rather than a module per unit.
    def apply(ctx: LeonContext, program: Program): Program = {
      def walkUnit(unit: UnitDef): UnitDef = {
        unit.copy(modules = Any1Ops.moduleDef +: unit.modules)
      }

      program.copy(units = program.units map walkUnit)
    }
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
