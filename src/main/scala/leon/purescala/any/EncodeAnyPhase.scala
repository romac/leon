/* Copyright 2009-2015 EPFL, Lausanne */

package leon
package purescala
package any

import Definitions.{Program, UnitDef}

object EncodeAnyPhase extends TransformationPhase {

  val name = "Encode Any"
  val description = "Encode Any as a sum type"

  def apply(ctx: LeonContext, program: Program): Program = {

    val Any1Ops = new Any1Ops(ctx, program)

    val phases =
      new ReplaceTypes(Any1Ops)           andThen
      new LiftExprs(Any1Ops)              andThen
      new AddModuleAnyToUnit(Any1Ops)

    phases.run(ctx)(program)
  }

  class AddModuleAnyToUnit(Any1Ops: Any1Ops) extends TransformationPhase {
    val name = "Add Module Any"
    val description = "Add module Any to program's unit"

    def walkUnit(unit: UnitDef, program: Program): UnitDef = {
      unit.copy(modules = Any1Ops.Any1ModuleDef +: unit.modules)
    }

    // FIXME: Should add a new unit for Any, rather than a module per unit.
    def apply(ctx: LeonContext, program: Program): Program = {
      program.copy(units = program.units.map(walkUnit(_, program)))
    }
  }
}
