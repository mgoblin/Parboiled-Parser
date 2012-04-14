package ru.mg.trace

import org.specs.SpecificationWithJUnit


class StatementTraceLineParserSpec extends SpecificationWithJUnit {

  val parser = new StatementTraceLineParser { override val buildParseTree = true }

  "Statement trace line parser" should {
    "parse statement trace line" in {

    }
  }
}
