
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

object DeclareAnyWrappers extends TransformationPhase {

  val name = "Declare Any Wrappers"
  val description = "Declare Any wrappers for all classes at the top of the hierarchy"

  def apply(ctx: LeonContext, program: Program): Program = {

    val any1ClassDef  = AbstractClassDef(FreshIdentifier("Any1"), Seq(), None)
    val any1ClassType = AbstractClassType(any1ClassDef, Seq())
    val any1Module    = ModuleDef(FreshIdentifier("Any1$Module"), Seq(any1ClassDef), false)

    var classDefsToAny1 = Map[ClassDef, CaseClassDef]()

    def wrapClass(cd: ClassDef): CaseClassDef = {
        val classDef  = CaseClassDef(FreshIdentifier("Any1$" + cd.id.name), Seq(), Some(any1ClassType), false).setPos(cd)
        val classType = classDefToClassType(classDef, Seq())

        val valueId   = FreshIdentifier("value", classType).setPos(cd)
        val field     = ValDef(valueId, Some(classType)).setPos(cd)

        classDef.setFields(Seq(field))

        any1ClassDef.registerChildren(classDef)
        classDefsToAny1 += (cd -> classDef)

        classDef
    }

    def walkUnit(u: UnitDef): UnitDef = {
      u.copy(modules = any1Module +: (u.modules map walkModule))
    }

    def walkModule(m: ModuleDef): ModuleDef = {
      val classesToWrap = m.classHierarchyRoots collect canBeWrapped
      val wrapperDefs =  classesToWrap map wrapClass

      m.copy(defs = m.defs ++ wrapperDefs)
    }

    def canBeWrapped: PartialFunction[ClassDef, ClassDef] = {
      case cd @ CaseClassDef(_, Seq(), _, _)  => cd
      case cd @ AbstractClassDef(_, Seq(), _) => cd
    }

    program.copy(units = program.units map walkUnit)
  }


}
