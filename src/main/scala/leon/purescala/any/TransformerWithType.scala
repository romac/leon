/* Copyright 2010-2015 EPFL, Lausanne */

package leon
package purescala

import Common._
import Expressions._
import ExprOps._
import Types._
import TypeOps._
import Definitions._
import Extractors._
import Constructors._

abstract class TransformerWithType extends Transformer {

  def transform(expr: Expr): Expr =
    transform(expr, expr.getType)

  def transform(expr: Expr, tpe: TypeTree): Expr = (expr match {
      case IfExpr(cond, thn, els) =>
        IfExpr(transform(cond),
               transform(thn, tpe),
               transform(els, tpe))

      case MatchExpr(scrut, cases) =>
        val newScrut = transform(scrut)
        val newCases = cases map { c =>
          val rhs   = transform(c.rhs, tpe)
          val pat   = transformPattern(c.pattern, scrut.getType)
          val guard = c.optGuard map transform

          MatchCase(pat, guard, rhs).copiedFrom(c)
        }

        MatchExpr(newScrut, newCases)

      case FunctionInvocation(tfd, args) =>
        val newArgs = transformArguments(args, tfd.fd.params)
        val funDef  = TypedFunDef(tfd.fd, tfd.tps.map(transformType))

        FunctionInvocation(funDef, newArgs)

      case LetDef(tfd, body) =>
        val newBody = transform(body, tfd.returnType)

        LetDef(tfd, newBody)

      case CaseClass(cct, args) =>
        val newTpe  = transformType(cct).asInstanceOf[CaseClassType]
        val newArgs = transformArguments(args, cct.fields)

        CaseClass(newTpe, newArgs)

      case FiniteSet(elements, base) =>
        val newBase     = transformType(base)
        val newElements = elements.map(transform(_, newBase))

        FiniteSet(newElements, newBase)

      /* TODO: Add other primites (Array, etc.) */

      case Error(errTpe, desc) =>
        Error(transformType(errTpe), desc)

      case v @ Variable(id) =>
        val newId = transformIdentifier(id)
        if (newId == id) v else Variable(id)

      case t: Terminal =>
        t

      case UnaryOperator(e, builder) =>
        builder(transform(e))

      case BinaryOperator(e1, e2, builder) =>
        builder(transform(e1), transform(e2))

      case NAryOperator(es, builder) =>
        builder(es map transform)

  }).copiedFrom(expr)

  def transformArguments(args: Seq[Expr], defs: Seq[ValDef]): Seq[Expr] = {
    require(args.length == defs.length)

    args zip defs map {
      case (l @ Lambda(args, body), vd) =>
        val funTpe  = vd.getType.asInstanceOf[FunctionType]
        val newBody = transform(body, funTpe.to)

        Lambda(args, newBody).copiedFrom(l)

      case (arg, vd) =>
        transform(arg, vd.getType)
    }
  }

  def transformIdentifier(id: Identifier): Identifier = id

  def transformPattern(pat: Pattern, scrutType: TypeTree): Pattern = pat

  def transformType(tpe: TypeTree): TypeTree = tpe

  def transformSubPatterns(subPatterns: Seq[Pattern], ct: ClassType): Seq[Pattern] = {
    subPatterns.zip(ct.fieldsTypes) map { case (pat, scrutTpe) =>
      transformPattern(pat, scrutTpe)
    }
  }

  def transformProgram(program: Program): Program = {
    program.definedFunctions foreach transformFunDef
    program
  }

  def transformFunDef(fd: FunDef): Unit = {
    fd.fullBody = transform(fd.fullBody, fd.returnType)
    fd.nestedFuns foreach transformFunDef
  }

}

