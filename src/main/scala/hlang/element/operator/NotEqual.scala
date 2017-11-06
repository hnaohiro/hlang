package hlang.element.operator

import hlang.Env
import hlang.element.{BinaryOperator, Element, Primitive}
import hlang.element.premitive.{BoolValue, FloatValue, IntValue, StringValue}
import hlang.error.{NotSupportedOperatorError, TypeMismatchError}

case class NotEqual(e1: Element, e2: Element) extends BinaryOperator {

  val character = "!="

  def eval(implicit env: Env): Primitive = (e1.eval, e2.eval) match {
    case (IntValue(v1), IntValue(v2))       => BoolValue(v1 != v2)
    case (IntValue(v1), FloatValue(v2))     => BoolValue(v1 != v2)
    case (FloatValue(v1), FloatValue(v2))   => BoolValue(v1 != v2)
    case (FloatValue(v1), IntValue(v2))     => BoolValue(v1 != v2)
    case (BoolValue(v1), BoolValue(v2))     => BoolValue(v1 != v2)
    case (StringValue(v1), StringValue(v2)) => BoolValue(v1 != v2)
    case (IntValue(_), v2)                  => throw TypeMismatchError(v2.pos, v2, IntValue.getClass)
    case (BoolValue(_), v2)                 => throw TypeMismatchError(v2.pos, v2, BoolValue.getClass)
    case (FloatValue(_), v2)                => throw TypeMismatchError(v2.pos, v2, FloatValue.getClass)
    case (StringValue(_), v2)               => throw TypeMismatchError(v2.pos, v2, StringValue.getClass)
    case (v1, _)                            => throw NotSupportedOperatorError(pos, character, v1)
  }
}
