package hlang.element.operator

import hlang.Env
import hlang.element.premitive.BoolValue
import hlang.element.{Element, Primitive, UnaryOperator}
import hlang.error.NotSupportedOperatorError

case class Not(e: Element) extends UnaryOperator {

  val character = "!"

  def eval(implicit env: Env): Primitive = e.eval match {
    case BoolValue(v) => BoolValue(!v)
    case v            => throw NotSupportedOperatorError(pos, character, v)
  }
}
