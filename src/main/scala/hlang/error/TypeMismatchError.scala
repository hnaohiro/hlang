package hlang.error

import hlang.element.Primitive

case class TypeMismatchError(found: Primitive, requiredType: Class[_], expression: String = "") extends Error {

  override def toString: String = {
    s"""
       |Type Mismatch Error:
       |  found:    $found
       |  required: ${requiredType.getSimpleName.dropRight(1)}
       |
       |> $expression
      """.stripMargin
  }
}
