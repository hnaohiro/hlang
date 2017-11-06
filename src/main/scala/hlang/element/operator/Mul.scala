package hlang.element.operator

import hlang.Env
import hlang.element.{BinaryOperator, Element, Primitive}
import hlang.element.premitive.{FloatValue, IntValue}
import hlang.error.{NotSupportedOperatorError, TypeMismatchError}

case class Mul(e1: Element, e2: Element) extends BinaryOperator {

  val character = "*"

  def eval(implicit env: Env): Primitive = (e1.eval, e2.eval) match {
    case (IntValue(v1), IntValue(v2))     => IntValue(v1 * v2)
    case (IntValue(v1), FloatValue(v2))   => FloatValue(v1 * v2)
    case (FloatValue(v1), FloatValue(v2)) => FloatValue(v1 * v2)
    case (FloatValue(v1), IntValue(v2))   => FloatValue(v1 * v2)
    case (IntValue(_), v2)                => throw TypeMismatchError(v2.pos, v2, IntValue.getClass)
    case (FloatValue(_), v2)              => throw TypeMismatchError(v2.pos, v2, FloatValue.getClass)
    case (v1, _)                          => throw NotSupportedOperatorError(pos, character, v1)
  }
}
