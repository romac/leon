/* Copyright 2009-2015 EPFL, Lausanne */

package leon

import leon.annotation._
import scala.language.implicitConversions

package object lang {

  @ignore
  type ?!? = Any

  @library
  implicit class BooleanDecorations(val underlying: Boolean) {
    def holds : Boolean = {
      assert(underlying)
      underlying
    }
    def ==> (that: Boolean): Boolean = {
      !underlying || that
    }
  }

  @ignore
  object InvariantFunction {
    def invariant(x: Boolean): Unit = ()
  }

  @ignore
  implicit def while2Invariant(u: Unit) = InvariantFunction

  @ignore
  def error[T](reason: java.lang.String): T = sys.error(reason)
 
  // @library
  // implicit class Passes[A,B](io : (A,B)) {
  //   val (in, out) = io
  //   def passes(tests : A => B ) : Boolean =
  //     try { tests(in) == out } catch { case _ : MatchError => true }
  // }

  @ignore
  object BigInt {
    def apply(b: Int): scala.math.BigInt = scala.math.BigInt(b)
    def apply(b: String): scala.math.BigInt = scala.math.BigInt(b)

    def unapply(b: scala.math.BigInt): scala.Option[Int] = {
      if(b >= Integer.MIN_VALUE && b <= Integer.MAX_VALUE) {
        scala.Some(b.intValue())
      } else {
        scala.None
      }
    }
  }

}
