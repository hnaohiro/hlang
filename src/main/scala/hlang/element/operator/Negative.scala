package hlang.element.operator

import hlang.element.{Element, Primitive, UnaryOperator}
import hlang.element.premitive.Number
import hlang.error.NotSupportedOperatorError

case class Negative(e: Element) extends UnaryOperator {

  val character = "-"

  def eval: Primitive = e.eval match {
    case Number(v) => Number(-v)
    case v         => throw NotSupportedOperatorError(this, v)
  }
}
