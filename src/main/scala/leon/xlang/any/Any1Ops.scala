/* Copyright 2009-2015 EPFL, Lausanne */

package leon
package xlang
package any

import leon.purescala._
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

  def registerChild(child: ClassDef): ClassDef = {
    classDef.registerChildren(child)
    child
  }

  def isAny(tpe: TypeTree): Boolean =
    tpe == AnyType || isSubtypeOf(tpe, classType)

  def isAny1(tpe: TypeTree): Boolean =
    tpe == classType

  def wrap(expr: Expr): Expr = expr.getType match {
    // TODO: Raise an error
    case tpe: TypeParameter =>
      println(s"Error: Cannot wrap generic type $tpe.")
      expr

    case tpe: ClassType if tpe.tps.nonEmpty =>
      println(s"Error: Cannot wrap polymorphic class $tpe")
      expr

    case tpe =>
      CaseClass(wrapperTypeFor(tpe), Seq(expr))
  }

  def wrapperTypeFor(tpe: TypeTree): CaseClassType = tpe match {
    case cTpe: ClassType =>
      val rootClass = rootClassDef(cTpe.classDef)
      val wrapperDef = classWrappers.getOrElseUpdate(rootClass, wrapClass(rootClass))
      classDefToClassType(wrapperDef).asInstanceOf[CaseClassType]

    case _ =>
      val wrapperDef = typeWrappers.getOrElseUpdate(tpe, wrapType(tpe))
      classDefToClassType(wrapperDef).asInstanceOf[CaseClassType]
  }

  def typeContainsAny(tpe: TypeTree): Boolean =
    typeExists(Any1Ops.isAny)(tpe)

  def mapTypeAnyToAny1(tpe: TypeTree, force: Boolean = false): TypeTree =
    if (force || typeContainsAny(tpe)) mapType(anyToAny1)(tpe)
    else tpe

  private def anyToAny1(tpe: TypeTree): Option[TypeTree] = tpe match {
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

    case _ if isAny1(tpe) => "Any1"

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
