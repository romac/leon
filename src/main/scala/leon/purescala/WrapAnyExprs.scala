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
      case (arg, vd) if Any1.isAny(vd.getType) =>
        wrapExprIfNeeded(arg)

      case (arg, _) => arg
    }
  }

  def wrapExprIfNeeded(e: Expr, tpe: TypeTree = Any1.classType): Expr = e match {
    case v @ Variable(id) if Any1.isAny(v.getType) && Any1.isAny(tpe) => any1Var(v)
    case _ if (Any1.shouldWrap(e, tpe))  => Any1.wrap(e)
    case _ => e
  }

  def any1Var(v: Variable): Variable = {
    val newId = FreshIdentifier(v.id.name, Any1.classType).copiedFrom(v.id)
    Variable(newId).copiedFrom(v)
  }

  def wrapExpr(e: Expr, tpe: TypeTree): Expr = e match {
    case IfExpr(cond, thenn, elze) if Any1.shouldWrap(e, tpe) =>
      IfExpr(wrapExpr(cond, cond.getType),
             wrapExpr(thenn, tpe),
             wrapExpr(elze, tpe)).copiedFrom(e)

    case m @ MatchExpr(scrut, cases) =>
      val expectedType =
        if (!m.isTyped || m.getType == AnyType) Any1.classType
        else m.getType

      val wcases = cases.map { c =>
        val wrhs = wrapExpr(c.rhs, expectedType)
        val wpat = wrapPattern(c.pattern, scrut.getType)
        val wguard = c.optGuard.map(g => wrapExpr(g, g.getType))
        MatchCase(wpat, wguard, wrhs).copiedFrom(c)
      }

      val wscrut = wrapExpr(scrut, scrut.getType)
      MatchExpr(wscrut, wcases).copiedFrom(m)

    case a @ AnyInstanceOf(cd: ClassType, v) =>
      CaseClassInstanceOf(Any1.wrapperTypeFor(cd), (wrapExprIfNeeded(v, tpe))).copiedFrom(a)

    case a @ AsInstanceOf(cd: ClassType, v) =>
      AsInstanceOf(Any1.wrapperTypeFor(cd), (wrapExprIfNeeded(v, tpe))).copiedFrom(a)

    case fi @ FunctionInvocation(tfd, args) if Any1.isAnyFunDef(tfd.fd) =>
      val newArgs = wrapArguments(args, tfd.fd.params)
      FunctionInvocation(tfd, newArgs).copiedFrom(fi)

    case mi @ MethodInvocation(rec, cd, tfd, args) if Any1.isAnyFunDef(tfd.fd) =>
      val newArgs = wrapArguments(args, tfd.fd.params)
      MethodInvocation(rec, cd, tfd, newArgs).copiedFrom(mi)

    case v @ Variable(id) if Any1.isAny(id.getType) =>
      any1Var(v)

    case t: Terminal if Any1.shouldWrap(e, tpe) =>
      Any1.wrap(t)

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

  def wrapPattern(pat: Pattern, tpe: TypeTree): Pattern = pat match {
    case InstanceOfPattern(binder, ct) if Any1.isAny(tpe) =>
      CaseClassPattern(None, Any1.wrapperTypeFor(ct), Seq(pat)).copiedFrom(pat)

    case CaseClassPattern(binder, ct, subPats) =>
      if (Any1.isAny(tpe))
        CaseClassPattern(None, Any1.wrapperTypeFor(ct), Seq(wrapPattern(pat, ct))).copiedFrom(pat)
      else
        CaseClassPattern(binder, ct, wrapSubPatterns(subPats, ct)).copiedFrom(pat)

    case TuplePattern(binder, subPatterns) =>
      pat // FIXME: What do we do with those?

    case WildcardPattern(binder) =>
      pat // FIXME: Make sure there's nothing to do here
  }

  def wrapSubPatterns(subPatterns: Seq[Pattern], ct: ClassType): Seq[Pattern] = {
    subPatterns.zip(ct.fieldsTypes) map { case (pat, tpe) =>
      wrapPattern(pat, tpe)
    }
  }

}
