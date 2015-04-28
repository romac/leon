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

    def transformValDef(vd: ValDef): Unit = {
      val newTpe = Any1Ops.mapTypeAnyToAny1(vd.getType)
      if (newTpe != vd.getType)
        vd.id.setType(newTpe)
    }

    def replaceTypes(fd: FunDef): Option[FunDef] = {
      val retType = Any1Ops.mapTypeAnyToAny1(fd.returnType)

      fd.params foreach transformValDef

      val newFd = new FunDef(fd.id.freshen, fd.tparams, retType, fd.params, fd.defType)
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
          args foreach transformValDef
          val newLambda = Lambda(args, processBody(body, fdMapCache)).copiedFrom(l)
          Some(newLambda)

        case _ => None
      }(e)

      replaceCalls(newBody)(fdMap)
    }

    val (prog, _) = replaceFunDefs(program)(replaceTypes)

    prog.definedClasses
      .collect { case cd: CaseClassDef => cd }
      .foreach { c =>
        c.fields foreach transformValDef
        c.setFields(c.fields)
      }

    prog
  }

}
