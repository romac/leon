/* Copyright 2009-2016 EPFL, Lausanne */

package leon
package genc

import converters._

import purescala.Definitions._
// NOTE don't import CAST._ to decrease possible confusion between the two ASTs

class CConverter(val ctx: LeonContext, val prog: Program)
extends Builder with Normalizer with TypeAnalyser with Converters with SimpleReporter {
  // Conversion entry point
  def convert: CAST.Prog = try {
    convertToProg
  } catch {
    case CAST.ConversionError(error, pos) =>
      val msg = s"GenC repported the following error:\n$error"
      ctx.reporter.fatalError(pos, msg)
  }

}

