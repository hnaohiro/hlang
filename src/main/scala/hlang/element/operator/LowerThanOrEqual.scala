package hlang.element.operator

import hlang.element.{BinaryOperator, Element, Primitive}
import hlang.element.premitive.{Bool, Number}
import hlang.error.{NotSupportedOperatorError, TypeMismatchError}

case class LowerThanOrEqual(e1: Element, e2: Element) extends BinaryOperator {

  val character = "<="

  def eval: Primitive = (e1.eval, e2.eval) match {
    case (Number(v1), Number(v2)) => Bool(v1 <= v2)
    case (Number(_), v2)          => throw TypeMismatchError(v2, Number.getClass, expression)
    case (v1, _)                  => throw NotSupportedOperatorError(this, v1)
  }
}
