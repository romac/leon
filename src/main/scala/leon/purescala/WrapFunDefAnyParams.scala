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

object WrapFunDefAnyParams extends TransformationPhase {

  val name = "Wrap FunDef's Any Params"
  val description = "Wrap FunDef's parameters of type Any into Any1"

  def apply(ctx: LeonContext, program: Program): Program = {

    def wrapFunDefTypes(fd: FunDef): Option[FunDef] = fd match {
      case _ if Any1.isAnyFunDef(fd) =>
        val retType = if (fd.returnType == AnyType) Any1.classType else fd.returnType

        val params = fd.params map {
          case vd if vd.getType == AnyType =>
            val newId = FreshIdentifier(vd.id.name, Any1.classType).copiedFrom(vd.id)
            ValDef(newId, Some(Any1.classType)).copiedFrom(vd)

          case vd => vd
        }

        val newFd = new FunDef(fd.id.freshen, fd.tparams, retType, params, fd.defType)
        newFd.copyContentFrom(fd)
        newFd.fullBody = wrapNestedFuns(newFd.fullBody)
        newFd.copiedFrom(fd)

        Some(newFd)

      case _ =>
        fd.fullBody = wrapNestedFuns(fd.fullBody)
        Some(fd)
    }

    // TODO: Reduce duplicated code by exposing fiMap and replaceCalls from DefOps.
    def fiMap(fi: FunctionInvocation, nfd: FunDef): Option[FunctionInvocation] = (fi, nfd) match {
      case (FunctionInvocation(old, args), newfd) if old.fd != newfd =>
        Some(FunctionInvocation(newfd.typed(old.tps), args))
      case _ =>
        None
    }

    def wrapNestedFuns(e: Expr): Expr = {
      var fdMapCache = Map[FunDef, FunDef]()
      def fdMap(fd: FunDef): FunDef = {
        fdMapCache.get(fd).getOrElse(fd)
      }

      def replaceCalls(e: Expr): Expr = {
        preMap {
          case fi @ FunctionInvocation(TypedFunDef(fd, tps), args) =>
            fiMap(fi, fdMap(fd)).map(_.setPos(fi))
          case _ =>
            None
        }(e)
      }

      val newBody = preMap {
        case ld @ LetDef(nfd, body) =>
          val wnfd = wrapFunDefTypes(nfd)
          wnfd.foreach(fdMapCache += nfd -> _)
          wnfd.map(LetDef(_, wrapNestedFuns(body)).copiedFrom(ld))

        case _ => None
      }(e)

      replaceCalls(newBody)
    }

    val (prog, fdMap) = replaceFunDefs(program)(wrapFunDefTypes)
    prog
  }

}
