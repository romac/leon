/* Copyright 2009-2015 EPFL, Lausanne */

package leon
package purescala

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
// - Make sure there's no problem with TypedFunDef's param member being a lazy val instead of a def
// - Extract re-usable part into TransformWithType tranformation
// - Make sure we map Any to Any1 wherever needed

object WrapAnyExprs extends TransformationPhase {

  val name = "Wrap Any Exprs"
  val description = "Wrap expressions into the appropriate Any1 subtype wherever needed"

  def apply(ctx: LeonContext, program: Program): Program = {
    def transformFunDef(fd: FunDef): Unit = {
      fd.fullBody = wrapExpr(fd.fullBody, fd.returnType)
      fd.nestedFuns foreach transformFunDef
    }

    program.definedFunctions foreach transformFunDef
    program
  }

  def wrapArguments(args: Seq[Expr], params: Seq[ValDef]): Seq[Expr] = {
    require(args.length == params.length)

    args.zip(params) map {
      case (arg, vd) if Any1Ops.typeContainsAny(vd.getType) =>
        wrapExpr(arg, vd.getType)

      case (l @ Lambda(args, body), vd) =>
        val funType = vd.getType.asInstanceOf[FunctionType]
        if (Any1Ops.typeContainsAny(funType.to))
          Lambda(args, wrapExpr(body, funType.to)).copiedFrom(l)
        else l

      case (arg, _) => arg
    }
  }

  def wrapExprIfNeeded(e: Expr, tpe: TypeTree = Any1Ops.classType): Expr =
    if (shouldWrap(e, tpe)) Any1Ops.wrap(e) else e

  def shouldWrap(e: Expr, tpe: TypeTree): Boolean =
    Any1Ops.isAny(tpe) && !Any1Ops.isAny1(e.getType)

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
      CaseClassInstanceOf(Any1Ops.wrapperTypeFor(cd), (wrapExpr(v, v.getType))).copiedFrom(a)

    case a @ AsInstanceOf(cd: ClassType, v) =>
      AsInstanceOf(Any1Ops.wrapperTypeFor(cd), (wrapExpr(v, v.getType))).copiedFrom(a)

    case fi @ FunctionInvocation(tfd, args) =>
      val newArgs = wrapArguments(args, tfd.fd.params)
      val funDef = TypedFunDef(tfd.fd, tfd.tps.map(Any1Ops.mapTypeAnyToAny1(_)))
      FunctionInvocation(funDef, newArgs).copiedFrom(fi)

    // Shouldn't be needed, we come after MethodLifting
    // case mi @ MethodInvocation(rec, cd, tfd, args) =>
    //   val newArgs = wrapArguments(args, tfd.fd.params)
    //   MethodInvocation(rec, cd, tfd, newArgs).copiedFrom(mi)

    case ld @ LetDef(tfd, body) =>
      LetDef(tfd, wrapExpr(body, tfd.returnType)).copiedFrom(ld)

    case v @ Variable(id) if Any1Ops.typeContainsAny(id.getType)=>
      id.setType(Any1Ops.mapTypeAnyToAny1(id.getType))
      v

    case es @ EmptySet(setType) if Any1Ops.typeContainsAny(setType) =>
      val newExpr = EmptySet(Any1Ops.mapTypeAnyToAny1(setType)).copiedFrom(es)
      wrapExprIfNeeded(newExpr, tpe)

    case ea @ EmptyArray(arrayType) if Any1Ops.typeContainsAny(arrayType) =>
      val newExpr = EmptyArray(Any1Ops.mapTypeAnyToAny1(arrayType)).copiedFrom(ea)
      wrapExprIfNeeded(newExpr, tpe)

    case em @ EmptyMap(kTpe, vTpe) if Any1Ops.typeContainsAny(kTpe) || Any1Ops.typeContainsAny(vTpe)  =>
      val newKTpe = Any1Ops.mapTypeAnyToAny1(kTpe)
      val newVTpe = Any1Ops.mapTypeAnyToAny1(vTpe)
      val newExpr = EmptyMap(newKTpe, newVTpe).copiedFrom(em)
      wrapExprIfNeeded(newExpr, tpe)

    case cc @ CaseClass(ccTpe, args) =>
      val newCcTpe = Any1Ops.mapTypeAnyToAny1(ccTpe).asInstanceOf[CaseClassType]
      val newCc = CaseClass(newCcTpe, wrapArguments(args, ccTpe.fields)).copiedFrom(cc)
      wrapExprIfNeeded(newCc, tpe)

    case t: Terminal =>
      wrapExprIfNeeded(t, tpe)

    case o @ UnaryOperator(e, builder) =>
      val res = builder(wrapExpr(e, e.getType))
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
    case InstanceOfPattern(binder, ct) if Any1Ops.isAny(scrutTpe) =>
      CaseClassPattern(None, Any1Ops.wrapperTypeFor(ct), Seq(pat)).copiedFrom(pat)

    case CaseClassPattern(_, ct, subPats) if Any1Ops.isAny(scrutTpe) =>
      CaseClassPattern(None, Any1Ops.wrapperTypeFor(ct), Seq(wrapPattern(pat, ct))).copiedFrom(pat)

    case CaseClassPattern(binder, ct, subPats) =>
      val newClassType = Any1Ops.mapTypeAnyToAny1(ct).asInstanceOf[CaseClassType]
      binder map (id => id.setType(Any1Ops.mapTypeAnyToAny1(id.getType)))
      CaseClassPattern(binder, newClassType, wrapSubPatterns(subPats, ct)).copiedFrom(pat)

    case PrimitivePattern(binder, tpe) => // scrutTpe should always be Any in that case
      val wrapperType = Any1Ops.wrapperTypeFor(tpe)
      CaseClassPattern(None, wrapperType, Seq(WildcardPattern(binder).copiedFrom(pat))).copiedFrom(pat)

    case TuplePattern(binder, subPats) =>
      val newBinder = binder map (id => id.setType(Any1Ops.mapTypeAnyToAny1(id.getType)))
      val newSubPatterns = subPats map (p => wrapPattern(p, Untyped))
      TuplePattern(newBinder, newSubPatterns).copiedFrom(pat)

    case WildcardPattern(binder) =>
      val newBinder = binder map (id => id.setType(Any1Ops.mapTypeAnyToAny1(id.getType)))
      WildcardPattern(newBinder).copiedFrom(pat)

    case LiteralPattern(binder, lit) if Any1Ops.isAny(scrutTpe) =>
      val wrapperType = Any1Ops.wrapperTypeFor(lit.getType)
      CaseClassPattern(None, wrapperType, Seq(pat)).copiedFrom(pat)

    case _ => pat
  }

  def wrapSubPatterns(subPatterns: Seq[Pattern], ct: ClassType): Seq[Pattern] = {
    subPatterns.zip(ct.fieldsTypes) map { case (pat, scrutTpe) =>
      wrapPattern(pat, scrutTpe)
    }
  }

}
