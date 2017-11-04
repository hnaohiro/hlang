package hlang.error

import hlang.element.{Operator, Primitive}

case class NotSupportedOperatorError(operator: Operator, found: Primitive) extends Error {

  override def toString: String = {
    s"""
        |Not Supported Operator Error: ${operator.expression}
        |  `${operator.character}` operator is not supported in $found
      """.stripMargin
  }
}
