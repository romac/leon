package leon
package purescala

import Common._
import Definitions._
import Expressions._
import Types._

object Any1 {

  val classDef  = AbstractClassDef(FreshIdentifier("Any1"), Seq(), None)
  val classType = AbstractClassType(classDef, Seq())
  val module    = ModuleDef(FreshIdentifier("Any1$Module"), Seq(classDef), false)

  private var map = Map[ClassDef, CaseClassDef]()

  def registerWrapper(cd: ClassDef, wrapper: CaseClassDef): Unit =
    map += (cd -> wrapper)

  def getWrapperFor(cd: ClassDef): Option[CaseClassDef] =
    map get cd

  def registerChild(cd: ClassDef): Unit =
    classDef.registerChildren(cd)

  def isAny(tpe: TypeTree): Boolean =
    tpe == AnyType || tpe == classType

  def isAnyFunDef(fd: FunDef): Boolean =
    isAny(fd.returnType) || fd.params.exists(p => isAny(p.getType))

}
