package hlang.element.operator

import hlang.Env
import hlang.element.premitive.BoolValue
import hlang.element.{BinaryOperator, Element, Primitive}
import hlang.error.{NotSupportedOperatorError, TypeMismatchError}

case class And(e1: Element, e2: Element) extends BinaryOperator {

  val character = "&&"

  def eval(implicit env: Env): Primitive = (e1.eval, e2.eval) match {
    case (BoolValue(v1), BoolValue(v2)) => BoolValue(v1 && v2)
    case (BoolValue(_), v2)             => throw TypeMismatchError(v2.pos, v2, BoolValue.getClass)
    case (v1, _)                        => throw NotSupportedOperatorError(pos, character, v1)
  }
}
