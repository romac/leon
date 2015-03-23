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
// - Also treat nested functions (done, but not in WrapFunDefAnyParams)

object WrapAnyExprs extends TransformationPhase {

  val name = "Wrap Any Exprs"
  val description = "Wrap expressions into the appropriate Any1 subtype wherever needed"

  def apply(ctx: LeonContext, program: Program): Program = {
    def transformFunDef(fd: FunDef): Unit = {
      fd.fullBody = transform(fd.fullBody, fd.returnType)
      fd.nestedFuns foreach transformFunDef
    }

    program.definedFunctions foreach transformFunDef
    program
  }

  def wrapArguments(args: Seq[Expr], params: Seq[ValDef]): Seq[Expr] = {
    require(args.length == params.length)

    args.zip(params) map {
      case (arg, vd) if Any1.isAny(vd.getType) =>
        wrap(arg)

      case (arg, _) => arg
    }
  }

  def shouldWrap(e: Expr, tpe: TypeTree): Boolean =
    Any1.isAny(tpe) && !Any1.isAny(e.getType)

  def wrap(e: Expr, tpe: TypeTree = Any1.classType): Expr =
    if (shouldWrap(e, tpe)) Any1.wrap(e) else e

  def transform(e: Expr, tpe: TypeTree): Expr = e match {
    case IfExpr(cond, thenn, elze) if shouldWrap(e, tpe) =>
      IfExpr(cond, transform(thenn, tpe), transform(elze, tpe)).copiedFrom(e)

    case m @ MatchExpr(scrut, cases) =>
      val expectedType =
        if (!m.isTyped || m.getType == AnyType) Any1.classType
        else m.getType

      val wcases = cases.map { c =>
        MatchCase(c.pattern, c.optGuard, transform(c.rhs, expectedType)).copiedFrom(c)
      }

      MatchExpr(scrut, wcases).copiedFrom(m)

    case a @ AnyInstanceOf(cd: ClassType, v) =>
      CaseClassInstanceOf(Any1.wrapperTypeFor(cd), (wrap(v, tpe))).copiedFrom(a)

    case a @ AsInstanceOf(cd: ClassType, v) =>
      AsInstanceOf(Any1.wrapperTypeFor(cd), (wrap(v, tpe))).copiedFrom(a)

    case fi @ FunctionInvocation(tfd, args) if Any1.isAnyFunDef(tfd.fd) =>
      val newArgs = wrapArguments(args, tfd.fd.params)
      FunctionInvocation(tfd, newArgs).copiedFrom(fi)

    case mi @ MethodInvocation(rec, cd, tfd, args) if Any1.isAnyFunDef(tfd.fd) =>
      val newArgs = wrapArguments(args, tfd.fd.params)
      MethodInvocation(rec, cd, tfd, newArgs).copiedFrom(mi)

    case t: Terminal if shouldWrap(e, tpe) =>
      Any1.wrap(t)

    case o @ UnaryOperator(e, builder) =>
      val res = builder(transform(e, e.getType))
      wrap(res, tpe)

    case o @ BinaryOperator(e1, e2, builder) =>
      val res = builder(transform(e1, e1.getType), transform(e2, e2.getType)).copiedFrom(o)
      wrap(res, tpe)

    case o @ NAryOperator(es, builder) =>
      val res = builder(es.map(e => transform(e, e.getType))).copiedFrom(o)
      wrap(res, tpe)

    case _ => e
  }

}
