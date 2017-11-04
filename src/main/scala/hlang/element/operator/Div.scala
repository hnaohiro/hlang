package hlang.element.operator

import hlang.element.{BinaryOperator, Element, Primitive}
import hlang.element.premitive.Number
import hlang.error.{NotSupportedOperatorError, TypeMismatchError}

case class Div(e1: Element, e2: Element) extends BinaryOperator {

  val character = "/"

  def eval: Primitive = (e1.eval, e2.eval) match {
    case (Number(v1), Number(v2)) => Number(v1 / v2)
    case (Number(_), v2)          => throw TypeMismatchError[Number](this, v2)
    case (v1, _)                  => throw NotSupportedOperatorError(this, v1)
  }
}
