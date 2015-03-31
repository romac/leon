/* Copyright 2009-2015 EPFL, Lausanne */

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

  def wrapperFor(cd: ClassDef): CaseClassDef =
    map(cd)

  def registerChild(cd: ClassDef): Unit =
    classDef.registerChildren(cd)

  def isAny(tpe: TypeTree): Boolean =
    tpe == AnyType || tpe == classType

  def isAnyFunDef(fd: FunDef): Boolean =
    isAny(fd.returnType) || fd.params.exists(p => isAny(p.getType))

  def shouldWrap(e: Expr, tpe: TypeTree): Boolean =
    isAny(tpe) && !isAny(e.getType)

  def wrap(e: Expr): Expr = e.getType match {
    case AnyType => e
    case Untyped => e // FIXME: Figure out what to do here
    case tpe: ClassType =>
      CaseClass(wrapperTypeFor(tpe), Seq(e))
  }

  def wrapperTypeFor(tpe: ClassType): CaseClassType = {
    val rootClass = rootClassDef(tpe.classDef)
    val any1Cd = map(rootClass)
    classDefToClassType(any1Cd).asInstanceOf[CaseClassType]
  }

  def rootClassDef(cd: ClassDef): ClassDef = cd.parent match {
    case None => cd
    case Some(parent) => rootClassDef(parent.classDef)
  }

}
