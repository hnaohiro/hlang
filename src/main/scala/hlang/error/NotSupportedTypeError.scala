package hlang.error

import scala.util.parsing.input.{NoPosition, Position}

case class NotSupportedTypeError(found: Class[_]) extends RuntimeError {

  val position: Position = NoPosition
  val message = s"${found.getSimpleName.dropRight(1)} is not supported"
}
