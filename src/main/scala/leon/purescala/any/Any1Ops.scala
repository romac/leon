/* Copyright 2009-2015 EPFL, Lausanne */

package leon
package purescala
package any

import Common._
import Definitions._
import Expressions._
import Types._
import ExprOps._
import TypeOps._
import DefOps._

import scala.collection.mutable.Map

class Any1Ops(ctx: LeonContext, program: Program) {

  private val constructors = Map[TypeTree, CaseClassDef]()

  def allConstructors: Seq[CaseClassDef] = constructors.values.toSeq

  lazy val Any1ClassDef: AbstractClassDef =
    program.library.Any1.get

  lazy val Any1ClassType: AbstractClassType =
    classDefToClassType(Any1ClassDef).asInstanceOf[AbstractClassType]

  lazy val UnexpectedDef: CaseClassDef =
    CaseClassDef(FreshIdentifier("Any1Unexpected"), Seq(), Some(Any1ClassType), true)

  lazy val Any1ModuleDef: ModuleDef = {
    Any1ClassDef.registerChildren(UnexpectedDef)
    val classDefs = UnexpectedDef +: allConstructors
    ModuleDef(FreshIdentifier("any1constructor"), classDefs, false)
  }

  def registerChild(child: ClassDef): ClassDef = {
    Any1ClassDef.registerChildren(child)
    child
  }

  def isAny(tpe: TypeTree): Boolean =
    tpe == AnyType || isSubtypeOf(tpe, Any1ClassType)

  def isAny1(tpe: TypeTree): Boolean =
    tpe == Any1ClassType

  def isUntyped(tpe: TypeTree): Boolean =
    tpe == Untyped

  def lift(expr: Expr): Expr = {
    if (!canBeLifted(expr.getType)) {
      // new Exception().printStackTrace()
      ctx.reporter.error(s"Cannot treat value ${expr} (${expr.getPos}) of type ${expr.getType} as Any")
      expr
    }
    else
      CaseClass(liftType(expr.getType), Seq(expr))
  }

  def canBeLifted(tpe: TypeTree): Boolean = tpe match {
    case tpe: TypeParameter                                                            => false
    case tpe: ClassType         if tpe.tps.nonEmpty                                    => false
    case SetType(base)          if typeContainsAny(base)                               => false
    case MultisetType(base)     if typeContainsAny(base)                               => false
    case MapType(from, to)      if typeContainsAny(from) || typeContainsAny(to)        => false
    case ArrayType(base)        if typeContainsAny(base)                               => false
    case FunctionType(from, to) if from.exists(typeContainsAny) || typeContainsAny(to) => false
    case _                                                                             => true
  }

  def liftType(tpe: TypeTree): CaseClassType = tpe match {
    case cTpe: ClassType =>
      val rootClass = rootClassDef(cTpe.classDef)
      val rootClassType = classDefToClassType(rootClass)
      val constructorDef = constructors.getOrElseUpdate(rootClassType, liftClass(rootClass))
      classDefToClassType(constructorDef).asInstanceOf[CaseClassType]

    case _ =>
      val constructorDef = constructors.getOrElseUpdate(tpe, liftPrimitive(tpe))
      classDefToClassType(constructorDef).asInstanceOf[CaseClassType]
  }

  def typeContainsAny(tpe: TypeTree): Boolean =
    typeExists(isAny)(tpe)

  def mapTypeAnyToAny1(tpe: TypeTree, force: Boolean = false): TypeTree =
    if (force || typeContainsAny(tpe)) mapType(anyToAny1)(tpe)
    else tpe

  private def anyToAny1(tpe: TypeTree): Option[TypeTree] = tpe match {
    case AnyType => Some(Any1ClassType)
    case _       => None
  }

  def liftClass(cd: ClassDef): CaseClassDef = {
    val constructor     = CaseClassDef(FreshIdentifier("Any1$" + cd.id.name), Seq(), Some(Any1ClassType), false).setPos(cd)
    val liftType = classDefToClassType(constructor, Seq())

    val valueType = classDefToClassType(cd)
    val valueId   = FreshIdentifier("value", valueType).setPos(cd)
    val field     = ValDef(valueId.setPos(cd))

    constructor.setFields(Seq(field))

    registerChild(constructor)
    constructors += classDefToClassType(cd) -> constructor

    constructor
  }

  def liftPrimitive(tpe: TypeTree): CaseClassDef = {
    val name        = typeName(tpe)
    val constructor = CaseClassDef(FreshIdentifier("Any1$" + name), Seq(), Some(Any1ClassType), false)
    val liftedType  = classDefToClassType(constructor, Seq())

    val valueId   = FreshIdentifier("value", tpe)
    val field     = ValDef(valueId)

    constructor.setFields(Seq(field))

    registerChild(constructor)
    constructors += tpe -> constructor

    constructor
  }

  def unliftExpr(expr: Expr): Expr = {
    def unlift(t: Expr): Option[Expr] = t match {
      case CaseClass(ct, value :: _) if isSubtypeOf(ct, Any1ClassType) =>
        Some(value)

      case _ => None
    }

    postMap(unlift)(expr)
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
