package hlang.error

import hlang.element.Primitive

import scala.util.parsing.input.Position

case class NotSupportedOperatorError(position: Position, operator: String, found: Primitive) extends RuntimeError {

  val message = s"`$operator` operator is not supported in $found"
}
