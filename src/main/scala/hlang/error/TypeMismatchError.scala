package hlang.error

import hlang.element.Primitive

import scala.util.parsing.input.Position

case class TypeMismatchError(position: Position, found: Primitive, requiredType: Class[_]) extends RuntimeError {

  val message: String = {
    val required = requiredType.getSimpleName.dropRight(1)
    s"type mismatch; $found is found, but $required is required"
  }
}
