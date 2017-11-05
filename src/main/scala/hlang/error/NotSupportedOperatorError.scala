package hlang.error

import hlang.element.Primitive

import scala.util.parsing.input.Position

case class NotSupportedOperatorError(position: Position, operator: String, found: Primitive) extends Error {

  override def toString: String = {
    s"""
        |Not Supported Operator Error: line: ${position.line}, column: ${position.column}
        |  `$operator` operator is not supported in $found
        |
        |```
        |${position.longString}
        |```
      """.stripMargin
  }
}
