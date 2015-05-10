/* Copyright 2009-2015 EPFL, Lausanne */

package leon
package purescala

import Common._
import Definitions._
import Types._

package object any {

  val Any1ClassDef  = AbstractClassDef(FreshIdentifier("Any1"), Seq(), None)
  val Any1ClassType = AbstractClassType(Any1ClassDef, Seq())

  lazy val UnexpectedDef = {
    val classDef = CaseClassDef(FreshIdentifier("Any1Unexpected"), Seq(), Some(Any1ClassType), true)
    Any1ClassDef.registerChildren(classDef)
    classDef
  }

  def Any1ModuleDef(wrappers: Seq[ClassDef]) = {
    val classDefs = Seq(Any1ClassDef, UnexpectedDef) ++ wrappers
    ModuleDef(FreshIdentifier("any1"), classDefs, false)
  }

}
