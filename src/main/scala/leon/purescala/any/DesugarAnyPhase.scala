/* Copyright 2009-2015 EPFL, Lausanne */

package leon
package purescala
package any

import Definitions.{Program, UnitDef}

object DesugarAnyPhase extends TransformationPhase {

  val name = "Desugar Any"
  val description = "Provide support for Any via case class wrappers"

  def apply(ctx: LeonContext, p: Program): Program = {

    val phases =
      ReplaceTypeAnyWithAny1 andThen
      WrapAnyExprs           andThen
      AddModuleAnyToUnit

    phases.run(ctx)(p)
  }

  object AddModuleAnyToUnit extends TransformationPhase {
    val name = "Add Module Any"
    val description = "Add module Any to program's unit"

    def walkUnit(unit: UnitDef): UnitDef = {
      unit.copy(modules = Any1ModuleDef(Any1Ops.allWrappers) +: unit.modules)
    }

    // FIXME: Should add a new unit for Any, rather than a module per unit.
    def apply(ctx: LeonContext, program: Program): Program = {
      program.copy(units = program.units map walkUnit)
    }
  }
}
