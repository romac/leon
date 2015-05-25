/* Copyright 2009-2015 EPFL, Lausanne */

package leon
package purescala
package any

import Definitions.{Program, UnitDef}

object DesugarAnyPhase extends TransformationPhase {

  val name = "Desugar Any"
  val description = "Provide support for Any via case class wrappers"

  def apply(ctx: LeonContext, program: Program): Program = {

    val any1Ops = new Any1Ops(ctx, program)

    val phases =
      new ReplaceTypeAnyWithAny1(any1Ops) andThen
      new WrapAnyExprs(any1Ops)           andThen
      new AddModuleAnyToUnit(any1Ops)

    phases.run(ctx)(program)
  }

  class AddModuleAnyToUnit(any1Ops: Any1Ops) extends TransformationPhase {
    val name = "Add Module Any"
    val description = "Add module Any to program's unit"

    def walkUnit(unit: UnitDef, program: Program): UnitDef = {
      unit.copy(modules = any1Ops.Any1ModuleDef +: unit.modules)
    }

    // FIXME: Should add a new unit for Any, rather than a module per unit.
    def apply(ctx: LeonContext, program: Program): Program = {
      program.copy(units = program.units.map(walkUnit(_, program)))
    }
  }
}
