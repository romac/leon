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
      case (arg, vd) if Any1Ops.isAny(vd.getType) =>
        wrapExpr(arg, vd.getType)

      case (l @ Lambda(args, body), vd) =>
        val funType = vd.getType.asInstanceOf[FunctionType]
        if (Any1Ops.isAny(funType.to)) {
          Lambda(args, wrapExpr(body, funType.to)).copiedFrom(l)
        }
        else l

      case (arg, _) => arg
    }
  }

  def wrapExprIfNeeded(e: Expr, tpe: TypeTree = Any1Ops.classType): Expr = e match {
    case _ if (Any1Ops.shouldWrap(e, tpe)) => Any1Ops.wrap(e)
    case _ => e
  }

  def wrapExpr(e: Expr, tpe: TypeTree): Expr = e match {
    case IfExpr(cond, thenn, elze) if Any1Ops.shouldWrap(e, tpe) =>
      IfExpr(wrapExpr(cond, cond.getType),
             wrapExpr(thenn, tpe),
             wrapExpr(elze, tpe)).copiedFrom(e)

    case m @ MatchExpr(scrut, cases) =>
      val expectedType =
        if (!m.isTyped || m.getType == AnyType) Any1Ops.classType
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
      CaseClassInstanceOf(Any1Ops.wrapperTypeFor(cd), (wrapExpr(v, v.getType))).copiedFrom(a)

    case a @ AsInstanceOf(cd: ClassType, v) =>
      AsInstanceOf(Any1Ops.wrapperTypeFor(cd), (wrapExpr(v, v.getType))).copiedFrom(a)

    case fi @ FunctionInvocation(tfd, args) =>
      val newArgs = wrapArguments(args, tfd.fd.params)
      val funDef = TypedFunDef(tfd.fd, tfd.tps.map(Any1Ops.mapTypeAnyToAny1(_)))
      FunctionInvocation(funDef, newArgs).copiedFrom(fi)

    case mi @ MethodInvocation(rec, cd, tfd, args) if Any1Ops.isAnyFunDef(tfd.fd) =>
      val newArgs = wrapArguments(args, tfd.fd.params)
      MethodInvocation(rec, cd, tfd, newArgs).copiedFrom(mi)

    case ld @ LetDef(tfd, body) =>
      LetDef(tfd, wrapExpr(body, tfd.returnType)).copiedFrom(ld)

    case v @ Variable(id) =>
      v.id.setType(Any1Ops.mapAnyToAny1(v.id.getType))
      v

    case es @ EmptySet(tpe) =>
      EmptySet(Any1Ops.mapAnyToAny1(tpe)).copiedFrom(es)

    case ea @ EmptyArray(tpe) =>
      EmptyArray(Any1Ops.mapAnyToAny1(tpe)).copiedFrom(ea)

    case em @ EmptyMap(kTpe, vTpe) =>
      val newKTpe = Any1Ops.mapAnyToAny1(kTpe)
      val newVTpe = Any1Ops.mapAnyToAny1(vTpe)
      EmptyMap(newKTpe, newVTpe).copiedFrom(em)

    case cc @ CaseClass(ccTpe, args) =>
      val newCcTpe = Any1Ops.mapAnyToAny1(ccTpe).asInstanceOf[CaseClassType]
      val newCc = CaseClass(newCcTpe, wrapArguments(args, ccTpe.fields)).copiedFrom(cc)
      wrapExprIfNeeded(newCc, tpe)

    case t: Terminal if Any1Ops.shouldWrap(e, tpe) =>
      Any1Ops.wrap(t)

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
    case InstanceOfPattern(binder, ct) if Any1Ops.isAny(tpe) =>
      CaseClassPattern(None, Any1Ops.wrapperTypeFor(ct), Seq(pat)).copiedFrom(pat)

    case CaseClassPattern(_, ct, subPats) if Any1Ops.isAny(tpe) =>
      CaseClassPattern(None, Any1Ops.wrapperTypeFor(ct), Seq(wrapPattern(pat, ct))).copiedFrom(pat)

    case CaseClassPattern(binder, ct, subPats) =>
      val newClassType = Any1Ops.mapAnyToAny1(ct).asInstanceOf[CaseClassType]
      binder map { id => id.setType(Any1Ops.mapAnyToAny1(id.getType)) }
      CaseClassPattern(binder, newClassType, wrapSubPatterns(subPats, ct)).copiedFrom(pat)

    case TuplePattern(binder, subPats) =>
      val newBinder = binder map { id => id.setType(Any1Ops.mapAnyToAny1(id.getType)) }
      val newSubPatterns = subPats map (p => wrapPattern(p, Untyped))
      TuplePattern(newBinder, newSubPatterns)

    case WildcardPattern(binder) =>
      val newBinder = binder map { id => id.setType(Any1Ops.mapAnyToAny1(id.getType)) }
      WildcardPattern(newBinder)
  }

  def wrapSubPatterns(subPatterns: Seq[Pattern], ct: ClassType): Seq[Pattern] = {
    subPatterns.zip(ct.fieldsTypes) map { case (pat, tpe) =>
      wrapPattern(pat, tpe)
    }
  }

}
