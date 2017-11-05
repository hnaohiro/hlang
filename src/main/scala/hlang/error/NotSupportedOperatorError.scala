package hlang.error

import hlang.element.{Operator, Primitive}

case class NotSupportedOperatorError(operator: Operator, found: Primitive) extends Error {

  override def toString: String = {
    s"""
        |Not Supported Operator Error:
        |  `${operator.character}` operator is not supported in $found
        |
        |> ${operator.expression}
      """.stripMargin
  }
}
