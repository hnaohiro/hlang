package hlang.error

import hlang.element.Primitive

import scala.util.parsing.input.Position

case class TypeMismatchError(position: Position, found: Primitive, requiredType: Class[_]) extends Error {

  override def toString: String = {
    s"""
       |Type Mismatch Error: line: ${position.line}, column: ${position.column}
       |  found:    $found
       |  required: ${requiredType.getSimpleName.dropRight(1)}
       |
       |```
       |${position.longString}
       |```
      """.stripMargin
  }
}
