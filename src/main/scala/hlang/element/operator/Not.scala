package hlang.element.operator

import hlang.element.premitive.Bool
import hlang.element.{Element, Primitive, UnaryOperator}
import hlang.error.NotSupportedOperatorError

case class Not(e: Element) extends UnaryOperator {

  val character = "!"

  def eval: Primitive = e.eval match {
    case Bool(v) => Bool(!v)
    case v       => throw NotSupportedOperatorError(this, v)
  }
}
