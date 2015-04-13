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

    // FIXME: We don't need to return an Option since we mutate the type directly.
    def transformValDef(vd: ValDef): Option[ValDef] = {
        val newTpe = mapType(transformType)(vd.getType)
        if (newTpe != vd.getType) {
          vd.id.setType(newTpe)
          None
        }
        else None
    }

    def replaceTypes(fd: FunDef): Option[FunDef] = {
      val retType = mapType(transformType)(fd.returnType)

      val params = fd.params map (vd => transformValDef(vd).getOrElse(vd))

      val newFd = new FunDef(fd.id.freshen, fd.tparams, retType, params, fd.defType)
      newFd.copyContentFrom(fd)
      newFd.fullBody = processBody(newFd.fullBody)
      newFd.copiedFrom(fd)

      Some(newFd)
    }

    def processBody(e: Expr, cache: Map[FunDef, FunDef] = Map()): Expr = {
      var fdMapCache = cache
      def fdMap(fd: FunDef): FunDef = {
        fdMapCache.get(fd).getOrElse(fd)
      }

      val newBody = preMap {
        case ld @ LetDef(nfd, body) =>
          val wnfd = replaceTypes(nfd)
          wnfd.foreach(fdMapCache += nfd -> _)
          wnfd.map(LetDef(_, processBody(body, fdMapCache)).copiedFrom(ld))

        case l @ Lambda(args, body) =>
          val newArgs = args map (arg => transformValDef(arg).getOrElse(arg))
          val newLambda = Lambda(newArgs, processBody(body, fdMapCache)).copiedFrom(l)
          Some(newLambda)

        case _ => None
      }(e)

      replaceCalls(newBody)(fdMap)
    }

    val (prog, _) = replaceFunDefs(program)(replaceTypes)

    prog.definedClasses
      .collect { case cd: CaseClassDef => cd }
      .foreach { c =>
        val fields = c.fields map (f => transformValDef(f).getOrElse(f))
        c.setFields(fields)
      }

    prog
  }

}
