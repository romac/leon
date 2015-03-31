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

object ReplaceTypeAnyWithAny1 extends TransformationPhase {

  val name = "Replace Type Any With Any1"
  val description = "Replace Any with Any1 in FunDef's return type and parameters"

  def apply(ctx: LeonContext, program: Program): Program = {

    def replaceTypes(fd: FunDef): Option[FunDef] = fd match {
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
        newFd.fullBody = processNestedFunctions(newFd.fullBody)
        newFd.copiedFrom(fd)

        Some(newFd)

      case _ =>
        fd.fullBody = processNestedFunctions(fd.fullBody)
        Some(fd)
    }

    def processNestedFunctions(e: Expr): Expr = {
      var fdMapCache = Map[FunDef, FunDef]()
      def fdMap(fd: FunDef): FunDef = {
        fdMapCache.get(fd).getOrElse(fd)
      }

      val newBody = preMap {
        case ld @ LetDef(nfd, body) =>
          val wnfd = replaceTypes(nfd)
          wnfd.foreach(fdMapCache += nfd -> _)
          wnfd.map(LetDef(_, processNestedFunctions(body)).copiedFrom(ld))

        case _ => None
      }(e)

      replaceCalls(newBody)(fdMap)
    }

    val (prog, fdMap) = replaceFunDefs(program)(replaceTypes)
    prog
  }

}
