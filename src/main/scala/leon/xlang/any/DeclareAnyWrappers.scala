
package leon
package xlang
package any

import leon.purescala._
import Common._
import Definitions._
import Expressions._
import Types._
import Extractors._
import ExprOps._
import DefOps._
import TypeOps._

object DeclareAnyWrappers extends TransformationPhase {

  val name = "Declare Any Wrappers"
  val description = "Declare Any wrappers for all classes at the top of the hierarchy"

  def apply(ctx: LeonContext, program: Program): Program = {

    def walkUnit(u: UnitDef): UnitDef = {
      val modules = u.modules map walkModule
      u.copy(modules = Any1Ops.moduleDef +: modules)
    }

    def walkModule(m: ModuleDef): ModuleDef = {
      val classesToWrap = m.classHierarchyRoots collect canBeWrapped
      val wrapperDefs =  classesToWrap map Any1Ops.wrapClass

      m.copy(defs = m.defs ++ wrapperDefs)
    }

    def canBeWrapped: PartialFunction[ClassDef, ClassDef] = {
      case cd @ CaseClassDef(_, Seq(), _, _)  => cd
      case cd @ AbstractClassDef(_, Seq(), _) => cd
    }

    program.copy(units = program.units map walkUnit)
  }


}
