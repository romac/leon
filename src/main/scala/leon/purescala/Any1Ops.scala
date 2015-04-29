/* Copyright 2009-2015 EPFL, Lausanne */

package leon
package purescala

import Common._
import Definitions._
import Expressions._
import Types._
import TypeOps._
import DefOps._

import scala.collection.mutable.Map

object Any1Ops {

  lazy val classDef      = AbstractClassDef(FreshIdentifier("Any1"), Seq(), None)
  lazy val classType     = AbstractClassType(classDef, Seq())
  lazy val moduleDef     = ModuleDef(FreshIdentifier("Any1$Module"), Seq(classDef, unexpectedDef), false)
  lazy val unexpectedDef = {
    val classDef = CaseClassDef(FreshIdentifier("Any1Unexpected"), Seq(), Some(classType), true)
    registerChild(classDef)
  }

  private val classWrappers = Map[ClassDef, CaseClassDef]()
  private val typeWrappers  = Map[TypeTree, CaseClassDef]()

  def registerWrapper(cd: ClassDef, wrapper: CaseClassDef): Unit =
    classWrappers += cd -> wrapper

  def registerWrapper(tpe: TypeTree, wrapper: CaseClassDef): Unit = {
    typeWrappers += tpe -> wrapper
  }

  def hasWrapper(tpe: ClassType): Boolean =
    classWrappers contains rootClassDef(tpe.classDef)

  def hasWrapper(tpe: TypeTree): Boolean =
    typeWrappers contains tpe

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
    case tpe: TypeParameter =>
      println(s"Error: Cannot wrap generic type $tpe.")
      e

    case tpe: ClassType if !tpe.tps.isEmpty =>
      println(s"Error: Cannot wrap polymorphic class $tpe")
      e

    case tpe: ClassType if hasWrapper(tpe) =>
      CaseClass(wrapperTypeFor(tpe), Seq(e))

    case tpe if hasWrapper(tpe) =>
      CaseClass(wrapperTypeFor(tpe), Seq(e))

    case tpe =>
      wrapType(tpe)
      wrap(e)
  }

  def wrapperTypeFor(tpe: ClassType): CaseClassType = {
    val rootClass = rootClassDef(tpe.classDef)
    val any1Cd = classWrappers(rootClass)
    classDefToClassType(any1Cd).asInstanceOf[CaseClassType]
  }

  def wrapperTypeFor(tpe: TypeTree): CaseClassType = {
    val wrapperDef = typeWrappers.getOrElseUpdate(tpe, wrapType(tpe))
    classDefToClassType(wrapperDef).asInstanceOf[CaseClassType]
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

  def wrapClass(cd: ClassDef): CaseClassDef = {
    val wrapper     = CaseClassDef(FreshIdentifier("Any1$" + cd.id.name), Seq(), Some(Any1Ops.classType), false).setPos(cd)
    val wrapperType = classDefToClassType(wrapper, Seq())

    val valueType = classDefToClassType(cd)
    val valueId   = FreshIdentifier("value", valueType).setPos(cd)
    val field     = ValDef(valueId.setPos(cd))

    wrapper.setFields(Seq(field))

    Any1Ops.registerChild(wrapper)
    Any1Ops.registerWrapper(cd, wrapper)

    wrapper
  }

  def wrapType(tpe: TypeTree): CaseClassDef = {
    val name        = typeName(tpe)
    val wrapper     = CaseClassDef(FreshIdentifier("Any1$" + name), Seq(), Some(Any1Ops.classType), false)
    val wrapperType = classDefToClassType(wrapper, Seq())

    val valueId   = FreshIdentifier("value", tpe)
    val field     = ValDef(valueId)

    wrapper.setFields(Seq(field))

    Any1Ops.registerChild(wrapper)
    Any1Ops.registerWrapper(tpe, wrapper)

    wrapper
  }

  private var typeId = 0
  private def typeName(tpe: TypeTree): String = tpe match {
    case Untyped             => "Untyped"
    case AnyType             => "Any"
    case BooleanType         => "Boolean"
    case UnitType            => "Unit"
    case CharType            => "Char"
    case IntegerType         => "BigInt"
    case Int32Type           => "Int"
    case BitVectorType(size) => "BitVector" + size

    case _ if tpe == classType => "Any1"

    case t @ TupleType(bases) =>
      "Tuple" + t.dimension + "$" + (bases.map(typeName).mkString("_"))
    case SetType(base) =>
      "Set$" + typeName(base)
    case MultisetType(base) =>
      "Multiset$" + typeName(base)
    case MapType(from, to) =>
      "Map$" + typeName(from) + "_" + typeName(to)
    case FunctionType(from, to) =>
      "Function" + from.map(typeName).mkString("_") + "$" + typeName(to)
    case ArrayType(base) =>
      "Array$" + typeName(base)
    case _ =>
      "Unknown" + {typeId += 1; typeId}
  }

}
