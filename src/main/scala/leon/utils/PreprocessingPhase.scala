/* Copyright 2009-2015 EPFL, Lausanne */

package leon
package utils

import purescala.Definitions.Program
import purescala.ScalaPrinter

import purescala.{MethodLifting, CompleteAbstractDefinitions, CheckADTFieldsTypes}
import purescala.{MethodLifting, CompleteAbstractDefinitions}
import xlang.any.DesugarAnyPhase
import synthesis.{ConvertWithOracle, ConvertHoles}
import verification.InjectAsserts

object PreprocessingPhase extends TransformationPhase {

  val name = "preprocessing"
  val description = "Various preprocessings on Leon programs"

  val optDesugarAny = LeonFlagOptionDef("any", "Enable support for Any", false)

  override val definedOptions: Set[LeonOptionDef[Any]] = Set(optDesugarAny)

  def apply(ctx: LeonContext, p: Program): Program = {
    val desugarAny = if (ctx.findOptionOrDefault(optDesugarAny)) DesugarAnyPhase
                     else NoopPhase[Program]()

    val phases =
      ScopingPhase                  andThen
      MethodLifting                 andThen
      desugarAny                    andThen
      TypingPhase                   andThen
      ConvertWithOracle             andThen
      ConvertHoles                  andThen
      CompleteAbstractDefinitions   andThen
      CheckADTFieldsTypes           andThen
      InjectAsserts


    phases.run(ctx)(p)
  }
}
