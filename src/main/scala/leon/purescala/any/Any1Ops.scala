/* Copyright 2009-2015 EPFL, Lausanne */

package leon
package purescala
package any

import Common._
import Definitions._
import Expressions._
import Types._
import TypeOps._
import DefOps._

import scala.collection.mutable.Map

object Any1Ops {

  private val classWrappers = Map[ClassDef, CaseClassDef]()
  private val typeWrappers  = Map[TypeTree, CaseClassDef]()

  def allWrappers: Seq[CaseClassDef] = classWrappers.values.toSeq ++ typeWrappers.values.toSeq

  def registerWrapper(cd: ClassDef, wrapper: CaseClassDef): Unit =
    classWrappers += cd -> wrapper

  def registerWrapper(tpe: TypeTree, wrapper: CaseClassDef): Unit = {
    typeWrappers += tpe -> wrapper
  }

  def registerChild(child: ClassDef): ClassDef = {
    Any1ClassDef.registerChildren(child)
    child
  }

  def isAny(tpe: TypeTree): Boolean =
    tpe == AnyType || isSubtypeOf(tpe, Any1ClassType)

  def isAny1(tpe: TypeTree): Boolean =
    tpe == Any1ClassType

  def wrap(expr: Expr)(implicit ctx: LeonContext): Expr = {
    if (!isWrappable(expr.getType)) {
      ctx.reporter.error(s"Cannot treat value of type ${expr.getType} as Any")
      expr
    }
    else
      CaseClass(wrapperTypeFor(expr.getType), Seq(expr))
  }

  def isWrappable(tpe: TypeTree): Boolean = tpe match {
    case tpe: TypeParameter =>
      false

    case tpe: ClassType if tpe.tps.nonEmpty =>
      false

    case SetType(base) if typeContainsAny(base) =>
      false

    case MultisetType(base) if typeContainsAny(base) =>
      false

    case MapType(from, to) if typeContainsAny(from) || typeContainsAny(to) =>
      false

    case ArrayType(base) if typeContainsAny(base) =>
      false

    case FunctionType(from, to) if from.exists(typeContainsAny) || typeContainsAny(to) =>
      false

    case _ =>
      true
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
    case AnyType => Some(Any1ClassType)
    case _       => None
  }

  def wrapClass(cd: ClassDef): CaseClassDef = {
    val wrapper     = CaseClassDef(FreshIdentifier("Any1$" + cd.id.name), Seq(), Some(Any1ClassType), false).setPos(cd)
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
    val wrapper     = CaseClassDef(FreshIdentifier("Any1$" + name), Seq(), Some(Any1ClassType), false)
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
