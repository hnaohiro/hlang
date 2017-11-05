package hlang.element.operator

import hlang.element.premitive.Bool
import hlang.element.{BinaryOperator, Element, Primitive}
import hlang.error.{NotSupportedOperatorError, TypeMismatchError}

case class And(e1: Element, e2: Element) extends BinaryOperator {

  val character = "&&"

  def eval: Primitive = (e1.eval, e2.eval) match {
    case (Bool(v1), Bool(v2)) => Bool(v1 && v2)
    case (Bool(_), v2)        => throw TypeMismatchError(pos, v2, Bool.getClass)
    case (v1, _)              => throw NotSupportedOperatorError(pos, character, v1)
  }
}
