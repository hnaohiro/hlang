package hlang.element.operator

import hlang.Env
import hlang.element.{Element, Primitive, UnaryOperator}
import hlang.element.premitive.{FloatValue, IntValue}
import hlang.error.NotSupportedOperatorError

case class Negative(e: Element) extends UnaryOperator {

  val character = "-"

  def eval(implicit env: Env): Primitive = e.eval match {
    case IntValue(v)   => IntValue(-v)
    case FloatValue(v) => FloatValue(-v)
    case v             => throw NotSupportedOperatorError(pos, character, v)
  }
}
