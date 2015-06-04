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

// TODO:
// - Gracefully handle possible errors (ie. when getting wrapper type or casting)
// - Extract re-usable part into TransformWithType tranformation

class WrapAnyExprs(any1Ops: Any1Ops) extends TransformationPhase {

  val name = "Wrap Any Exprs"
  val description = "Wrap expressions into the appropriate Any1 subtype wherever needed"

  def apply(ctx: LeonContext, program: Program): Program = {

    def wrapArguments(args: Seq[Expr], params: Seq[ValDef]): Seq[Expr] = {
      require(args.length == params.length)

      args.zip(params) map {
        case (arg, vd) if any1Ops.typeContainsAny(vd.getType) =>
          wrapExpr(arg, vd.getType)

        case (l @ Lambda(args, body), vd) =>
          val funType = vd.getType.asInstanceOf[FunctionType]
          if (any1Ops.typeContainsAny(funType.to))
            Lambda(args, wrapExpr(body, funType.to)).copiedFrom(l)
          else l

        case (arg, _) => arg
      }
    }

    def wrapExprIfNeeded(e: Expr, tpe: TypeTree = any1Ops.Any1ClassType): Expr =
      if (shouldWrap(e, tpe)) any1Ops.wrap(e) else e

    def shouldWrap(e: Expr, tpe: TypeTree): Boolean =
      any1Ops.isAny(tpe) && !any1Ops.isAny1(e.getType)

    def wrapExpr(e: Expr, tpe: TypeTree): Expr = e match {
      case i @ IfExpr(cond, thenn, elze) if shouldWrap(i, tpe) =>
        IfExpr(wrapExpr(cond, cond.getType),
               wrapExpr(thenn, tpe),
               wrapExpr(elze, tpe)).copiedFrom(i)

      case m @ MatchExpr(scrut, cases) =>
        val wcases = cases.map { c =>
          val wrhs = wrapExpr(c.rhs, tpe)
          val wpat = wrapPattern(c.pattern, scrut.getType)
          val wguard = c.optGuard.map(g => wrapExpr(g, g.getType))
          MatchCase(wpat, wguard, wrhs).copiedFrom(c)
        }

        val wscrut = wrapExpr(scrut, scrut.getType)
        MatchExpr(wscrut, wcases).copiedFrom(m)

      case a @ AnyInstanceOf(cd: ClassType, v) =>
        CaseClassInstanceOf(any1Ops.wrapperTypeFor(cd), (wrapExpr(v, v.getType))).copiedFrom(a)

      case a @ AsInstanceOf(cd: ClassType, v) =>
        AsInstanceOf(any1Ops.wrapperTypeFor(cd), (wrapExpr(v, v.getType))).copiedFrom(a)

      case e @ Error(tpe, desc) =>
        Error(any1Ops.mapTypeAnyToAny1(tpe), desc).copiedFrom(e)

      case fi @ FunctionInvocation(tfd, args) =>
        val newArgs = wrapArguments(args, tfd.fd.params)
        val funDef = TypedFunDef(tfd.fd, tfd.tps.map(any1Ops.mapTypeAnyToAny1(_)))
        val newFi = FunctionInvocation(funDef, newArgs).copiedFrom(fi)
        wrapExprIfNeeded(newFi, tpe)

      // Shouldn't be needed as we come after MethodLifting
      // case mi @ MethodInvocation(rec, cd, tfd, args) =>
      //   val newArgs = wrapArguments(args, tfd.fd.params)
      //   val funDef = TypedFunDef(tfd.fd, tfd.tps.map(any1Ops.mapTypeAnyToAny1(_)))
      //   MethodInvocation(rec, cd, funDef, newArgs).copiedFrom(mi)

      case ld @ LetDef(tfd, body) =>
        LetDef(tfd, wrapExpr(body, tfd.returnType)).copiedFrom(ld)

      case v @ Variable(id) if any1Ops.typeContainsAny(id.getType)=>
        id.setType(any1Ops.mapTypeAnyToAny1(id.getType))
        v

      case cc @ CaseClass(ccTpe, args) =>
        val newCcTpe = any1Ops.mapTypeAnyToAny1(ccTpe).asInstanceOf[CaseClassType]
        val newCc = CaseClass(newCcTpe, wrapArguments(args, ccTpe.fields)).copiedFrom(cc)
        wrapExprIfNeeded(newCc, tpe)

      case s @ FiniteSet(elements, base) =>
        val newBase = any1Ops.mapTypeAnyToAny1(base)
        val newElems = elements.map(wrapExpr(_, newBase))
        FiniteSet(newElems, newBase).copiedFrom(s)

      case r @ Require(pred, body) =>
        r
        // Require(wrapExpr(pred, pred.getType),
        //         wrapExpr(body, body.getType)).copiedFrom(r)

      case e @ Ensuring(body, pred) =>
        e
        // Ensuring(wrapExpr(body, body.getType),
        //          wrapExpr(pred, pred.getType)).copiedFrom(e)

      case t: Terminal =>
        wrapExprIfNeeded(t, tpe).copiedFrom(t)

      case o @ UnaryOperator(e, builder) =>
        val res = builder(wrapExpr(e, e.getType)).copiedFrom(o)
        wrapExprIfNeeded(res, tpe)

      case o @ BinaryOperator(e1, e2, builder) =>
        val res = builder(wrapExpr(e1, e1.getType), wrapExpr(e2, e2.getType)).copiedFrom(o)
        wrapExprIfNeeded(res, tpe)

      case o @ NAryOperator(es, builder) =>
        val res = builder(es.map(e => wrapExpr(e, e.getType))).copiedFrom(o)
        wrapExprIfNeeded(res, tpe)

      case _ => e
    }

    def wrapPattern(pat: Pattern, scrutTpe: TypeTree): Pattern = pat match {
      case InstanceOfPattern(binder, ct) if any1Ops.isAny(scrutTpe) =>
        CaseClassPattern(None, any1Ops.wrapperTypeFor(ct), Seq(pat)).copiedFrom(pat)

      case CaseClassPattern(_, ct, subPats) if any1Ops.isAny(scrutTpe) =>
        CaseClassPattern(None, any1Ops.wrapperTypeFor(ct), Seq(wrapPattern(pat, ct))).copiedFrom(pat)

      case CaseClassPattern(binder, ct, subPats) =>
        val newClassType = any1Ops.mapTypeAnyToAny1(ct).asInstanceOf[CaseClassType]
        binder map (id => id.setType(any1Ops.mapTypeAnyToAny1(id.getType)))
        CaseClassPattern(binder, newClassType, wrapSubPatterns(subPats, ct)).copiedFrom(pat)

      case PrimitivePattern(binder, tpe) => // scrutTpe should always be Any in that case
        val wrapperType = any1Ops.wrapperTypeFor(tpe)
        CaseClassPattern(None, wrapperType, Seq(WildcardPattern(binder).copiedFrom(pat))).copiedFrom(pat)

      case TuplePattern(binder, subPats) if any1Ops.isAny(scrutTpe) =>
        val patType     = patternType(pat)
        val wrapperType = any1Ops.wrapperTypeFor(patType)
        val newPat      = wrapPattern(pat, patType)
        CaseClassPattern(None, wrapperType, Seq(newPat)).copiedFrom(pat)

      case TuplePattern(binder, subPats) =>
        val newBinder = binder map (id => id.setType(any1Ops.mapTypeAnyToAny1(id.getType)))
        val newSubPatterns = subPats map (p => wrapPattern(p, Untyped))
        TuplePattern(newBinder, newSubPatterns).copiedFrom(pat)

      case WildcardPattern(binder) =>
        val newBinder = binder map { id =>
          id.setType(any1Ops.mapTypeAnyToAny1(id.getType))
        }
        WildcardPattern(newBinder).copiedFrom(pat)

      case LiteralPattern(binder, lit) if any1Ops.isAny(scrutTpe) =>
        val wrapperType = any1Ops.wrapperTypeFor(lit.getType)
        CaseClassPattern(None, wrapperType, Seq(pat)).copiedFrom(pat)

      case _ => pat
    }

    def wrapSubPatterns(subPatterns: Seq[Pattern], ct: ClassType): Seq[Pattern] = {
      subPatterns.zip(ct.fieldsTypes) map { case (pat, scrutTpe) =>
        wrapPattern(pat, scrutTpe)
      }
    }

    def transformFunDef(fd: FunDef): Unit = {
      fd.fullBody = wrapExpr(fd.fullBody, fd.returnType)
      fd.nestedFuns foreach transformFunDef
    }

    program.definedFunctions foreach transformFunDef
    program
  }

}
