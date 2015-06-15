/* Copyright 2009-2015 EPFL, Lausanne */

package leon
package purescala
package any

import Common._
import Definitions._
import Expressions._
import Types._
import Extractors._
import ExprOps._
import DefOps._
import TypeOps._

class LiftExprs(Any1Ops: Any1Ops) extends TransformationPhase {

  val name = "Lift Expressions to Any1"
  val description = "Lift expressions into the Any1 sum type by wrapping them with the appropriate constructor"

  def apply(ctx: LeonContext, program: Program): Program = {
    LiftExprTransformer.transformProgram(program)
  }

  object LiftExprTransformer extends TransformerWithType {

    def liftExpr(expr: Expr, tpe: TypeTree = Any1Ops.Any1ClassType, ifNeeded: Boolean = true): Expr =
      if (ifNeeded && needsWrapper(expr, tpe)) Any1Ops.lift(expr)
      else expr

    def needsWrapper(expr: Expr, tpe: TypeTree): Boolean =
      Any1Ops.isAny(tpe) && !Any1Ops.isAny1(expr.getType)

    override
    def transformType(tpe: TypeTree): TypeTree =
      Any1Ops.mapTypeAnyToAny1(tpe)

    override
    def transformPattern(pat: Pattern, scrutTpe: TypeTree): Pattern = (pat match {

      case InstanceOfPattern(binder, ct) if Any1Ops.isAny(scrutTpe) || Any1Ops.isUntyped(scrutTpe) =>
        val wrapperTpe = Any1Ops.liftType(ct)
        CaseClassPattern(None, wrapperTpe, Seq(pat))

      case CaseClassPattern(_, ct, _) if Any1Ops.isAny(scrutTpe) =>
        val wrapperTpe = Any1Ops.liftType(ct)
        val newPat     = transformPattern(pat, ct)

        CaseClassPattern(None, wrapperTpe, Seq(newPat))

      case CaseClassPattern(binder, ct, subPats) =>
        val newCt      = Any1Ops.mapTypeAnyToAny1(ct).asInstanceOf[CaseClassType]
        val newSubPats = transformSubPatterns(subPats, ct)
        val newBinder  = binder map transformIdentifier

        CaseClassPattern(newBinder, newCt, newSubPats)

      // scrutTpe should always be Any/Untyped in that case
      case PrimitivePattern(binder, tpe) =>
        require(Any1Ops.isAny(scrutTpe) || Any1Ops.isUntyped(scrutTpe))

        val wrapperTpe = Any1Ops.liftType(tpe)
        val newPat     = WildcardPattern(binder).copiedFrom(pat)

        CaseClassPattern(None, wrapperTpe, Seq(newPat))

      case TuplePattern(binder, subPats) if Any1Ops.isAny(scrutTpe) =>
        val patType    = patternType(pat)
        val wrapperTpe = Any1Ops.liftType(patType)
        val newPat     = transformPattern(pat, patType)

        CaseClassPattern(None, wrapperTpe, Seq(newPat)).copiedFrom(pat)

      case TuplePattern(binder, subPats) =>
        val newBinder      = binder map transformIdentifier
        val newSubPatterns = subPats map (transformPattern(_, Untyped))

        TuplePattern(newBinder, newSubPatterns).copiedFrom(pat)

      case WildcardPattern(binder) =>
        val newBinder = binder map transformIdentifier
        WildcardPattern(newBinder)

      case LiteralPattern(binder, lit) if Any1Ops.isAny(scrutTpe) =>
        val wrapperTpe = Any1Ops.liftType(lit.getType)
        CaseClassPattern(None, wrapperTpe, Seq(pat))

      case _ =>
        super.transformPattern(pat, scrutTpe)

    }).copiedFrom(pat)

    override
    def transformIdentifier(id: Identifier): Identifier =
      id.setType(transformType(id.getType))

    override
    def transform(expr: Expr, tpe: TypeTree): Expr = expr match {

      case AnyInstanceOf(cd: ClassType, v) =>
        val wrapperTpe = Any1Ops.liftType(cd)
        val newSubject = transform(v)
        val newExpr    = CaseClassInstanceOf(wrapperTpe, newSubject)

        liftExpr(newExpr, tpe)

      case _ =>
        val newExpr = super.transform(expr, tpe)

        liftExpr(newExpr, tpe)
    }
  }

}
