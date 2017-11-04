package hlang.error

import hlang.element.{Operator, Primitive}

import scala.reflect.ClassTag

case class TypeMismatchError[T <: Primitive](operator: Operator, found: Primitive)(implicit tag: ClassTag[T])
    extends Error {

  override def toString: String = {
    s"""
       |Type Mismatch Error: ${operator.expression}
       |  found:    $found
       |  required: ${tag.runtimeClass.getSimpleName}
      """.stripMargin
  }
}
