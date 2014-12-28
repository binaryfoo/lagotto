package io.github.binaryfoo.lagotto.shell

import io.github.binaryfoo.lagotto._

case class Config (filters: Seq[LogFilter] = Seq(),
                   input: Seq[String] = Seq(),
                   format: OutputFormat = FullText,
                   pair: Boolean = false,
                   header: Boolean = true,
                   beforeContext: Int = 0,
                   afterContext: Int = 0,
                   sortBy: Option[GroundedFieldExpr] = None,
                   sortDescending: Boolean = false,
                   strict: Boolean = false,
                   progressMeter: ProgressMeter = NullProgressMeter,
                   histogramFields: Seq[String] = Seq(),
                   gnuplotFileName: Option[String] = None)
