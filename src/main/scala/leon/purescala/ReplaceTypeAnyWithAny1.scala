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

    // TODO: Keep track of functions that returns Any and replace their invocations
    def transformType(t: TypeTree): Option[TypeTree] = t match {
      case AnyType => Some(Any1Ops.classType)
      case t       => None
    }

    def replaceTypes(fd: FunDef): Option[FunDef] = {
      val retType = mapType(transformType)(fd.returnType)

      val params = fd.params map { vd =>
        val newTpe = mapType(transformType)(vd.getType)
        if (newTpe != vd.getType)
          ValDef(FreshIdentifier(vd.id.name, newTpe).copiedFrom(vd.id)).copiedFrom(vd)
        else
          vd
      }

      val newFd = new FunDef(fd.id.freshen, fd.tparams, retType, params, fd.defType)
      newFd.copyContentFrom(fd)
      newFd.fullBody = processBody(newFd.fullBody)
      newFd.copiedFrom(fd)

      Some(newFd)
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
