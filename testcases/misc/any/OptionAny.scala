/* Copyright 2009-2015 EPFL, Lausanne */

package leon.collection.any

import leon.annotation._

// @library
sealed abstract class OptionAny {

  def get: Any = {
    require(this.isDefined)
    this match {
      case Some(x) => x
    }
  }

  def getOrElse(default: Any) = this match {
    case Some(v) => v
    case None()  => default
  }

  def orElse(or: OptionAny) = this match {
    case Some(v) => this
    case None() => or
  }

  def isEmpty = this match {
    case Some(v) => false
    case None() =>  true
  }

  def nonEmpty  = !isEmpty

  def isDefined = !isEmpty


  // Higher-order API
  def map(f: Any => Any) = { this match {
    case None() => None()
    case Some(x) => Some(f(x))
  }} ensuring { _.isDefined == this.isDefined }

  def flatMap(f: Any => OptionAny) = this match {
    case None() => None()
    case Some(x) => f(x)
  }

  def filter(p: Any => Boolean) = this match {
    case Some(x) if p(x) => Some(x)
    case _ => None()
  }

  def withFilter(p: Any => Boolean) = filter(p)

  def forall(p: Any => Boolean) = this match {
    case Some(x) if !p(x) => false
    case _ => true
  }

  def exists(p: Any => Boolean) = !forall(!p(_))

}

case class Some(v: Any) extends OptionAny
case class None() extends OptionAny
