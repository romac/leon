/* Copyright 2009-2015 EPFL, Lausanne */

package leon
package purescala

import Common._
import Definitions._
import Expressions._
import Types._
import TypeOps._

// TODO: Figure out why no warning are raised if we don't match on Unexpected
object Any1Ops {

  val classDef      = AbstractClassDef(FreshIdentifier("Any1"), Seq(), None)
  val classType     = AbstractClassType(classDef, Seq())
  val unexpectedDef = registerChild(CaseClassDef(FreshIdentifier("Any1Unexpected"), Seq(), Some(classType), true))
  val moduleDef     = ModuleDef(FreshIdentifier("Any1$Module"), Seq(classDef, unexpectedDef), false)

  private var wrapperMap = Map[ClassDef, CaseClassDef]()

  def registerWrapper(cd: ClassDef, wrapper: CaseClassDef): Unit =
    wrapperMap += cd -> wrapper

  def getWrapperFor(cd: ClassDef): Option[CaseClassDef] =
    wrapperMap get cd

  def wrapperFor(cd: ClassDef): CaseClassDef =
    wrapperMap(cd)

  def registerChild(child: ClassDef): ClassDef = {
    classDef.registerChildren(child)
    child
  }

  def isAny(tpe: TypeTree): Boolean =
    tpe == AnyType || isSubtypeOf(tpe, classType)

  def isAny1(tpe: TypeTree): Boolean =
    tpe == classType

  def isAnyFunDef(fd: FunDef): Boolean =
    isAny(fd.returnType) || fd.params.exists(p => isAny(p.getType))

  def shouldWrap(e: Expr, tpe: TypeTree): Boolean =
    isAny(tpe) && !isAny1(e.getType)

  def wrap(e: Expr): Expr = e.getType match {
    // case AnyType => e
    // case Untyped => e // FIXME: Figure out what to do here
    // case tpe: TypeParameter => e
    case tpe: ClassType if wrapperMap contains rootClassDef(tpe.classDef) =>
      CaseClass(wrapperTypeFor(tpe), Seq(e))
    case _  => e
  }

  def wrapperTypeFor(tpe: ClassType): CaseClassType = {
    val rootClass = rootClassDef(tpe.classDef)
    val any1Cd = wrapperMap(rootClass)
    classDefToClassType(any1Cd).asInstanceOf[CaseClassType]
  }

  def rootClassDef(cd: ClassDef): ClassDef = cd.parent match {
    case None => cd
    case Some(parent) => rootClassDef(parent.classDef)
  }

  def anyToAny1(t: TypeTree): Option[TypeTree] = t match {
    case AnyType => Some(Any1Ops.classType)
    case t       => None
  }

  def mapAnyToAny1(t: TypeTree): TypeTree =
    mapType(anyToAny1)(t)
}
