/* Copyright 2009-2015 EPFL, Lausanne */

package leon
package purescala

import Common._
import Definitions._
import Expressions._
import Types._
import TypeOps._

object Any1Ops {

  lazy val classDef      = AbstractClassDef(FreshIdentifier("Any1"), Seq(), None)
  lazy val classType     = AbstractClassType(classDef, Seq())
  lazy val moduleDef     = ModuleDef(FreshIdentifier("Any1$Module"), Seq(classDef, unexpectedDef), false)
  lazy val unexpectedDef = {
    val classDef = CaseClassDef(FreshIdentifier("Any1Unexpected"), Seq(), Some(classType), true)
    registerChild(classDef)
  }

  private var wrappers = Map[ClassDef, CaseClassDef]()

  def registerWrapper(cd: ClassDef, wrapper: CaseClassDef): Unit =
    wrappers += cd -> wrapper

  def hasWrapper(tpe: ClassType): Boolean =
    wrappers contains rootClassDef(tpe.classDef)

  def registerChild(child: ClassDef): ClassDef = {
    classDef.registerChildren(child)
    child
  }

  def isAny(tpe: TypeTree): Boolean =
    tpe == AnyType || isSubtypeOf(tpe, classType)

  def isAny1(tpe: TypeTree): Boolean =
    tpe == classType

  def wrap(e: Expr): Expr = e.getType match {
    // TODO: Raise an error
    case tpe: TypeParameter => e

    case tpe: ClassType if hasWrapper(tpe) =>
      CaseClass(wrapperTypeFor(tpe), Seq(e))

    case tpe =>
      println(s"warning: no wrapper for type $tpe")
      e
  }

  def wrapperTypeFor(tpe: ClassType): CaseClassType = {
    val rootClass = rootClassDef(tpe.classDef)
    val any1Cd = wrappers(rootClass)
    classDefToClassType(any1Cd).asInstanceOf[CaseClassType]
  }

  private def rootClassDef(cd: ClassDef): ClassDef = cd.parent match {
    case None         => cd
    case Some(parent) => rootClassDef(parent.classDef)
  }

  def typeContainsAny(t: TypeTree): Boolean =
    typeExists(Any1Ops.isAny)(t)

  def mapTypeAnyToAny1(t: TypeTree, force: Boolean = false): TypeTree =
    if (force || typeContainsAny(t))
      mapType(anyToAny1)(t)
    else
      t

  private def anyToAny1(t: TypeTree): Option[TypeTree] = t match {
    case AnyType => Some(Any1Ops.classType)
    case _       => None
  }

}
