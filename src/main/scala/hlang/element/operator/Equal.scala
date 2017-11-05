package hlang.element.operator

import hlang.element.{BinaryOperator, Element, Primitive}
import hlang.element.premitive.{Bool, Number}
import hlang.error.{NotSupportedOperatorError, TypeMismatchError}

case class Equal(e1: Element, e2: Element) extends BinaryOperator {

  val character = "=="

  def eval: Primitive = (e1.eval, e2.eval) match {
    case (Bool(v1), Bool(v2))     => Bool(v1 == v2)
    case (Number(v1), Number(v2)) => Bool(v1 == v2)
    case (Bool(_), v2)            => throw TypeMismatchError(pos, v2, Bool.getClass)
    case (Number(_), v2)          => throw TypeMismatchError(pos, v2, Number.getClass)
    case (v1, _)                  => throw NotSupportedOperatorError(pos, character, v1)
  }
}
