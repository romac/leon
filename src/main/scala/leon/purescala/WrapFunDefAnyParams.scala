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

// FIXME: replaceFunDefs doesn't replace inner FunDefs nor MethodInvocations
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
        newFd.copiedFrom(fd)

        Some(newFd)

      case _ => None
    }

    val (prog, fdMap) = replaceFunDefs(program)(wrapFunDefTypes)
    prog
  }

}
