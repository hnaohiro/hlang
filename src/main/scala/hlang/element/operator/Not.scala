package hlang.element.operator

import hlang.Env
import hlang.element.premitive.Bool
import hlang.element.{Element, Primitive, UnaryOperator}
import hlang.error.NotSupportedOperatorError

case class Not(e: Element) extends UnaryOperator {

  val character = "!"

  def eval(implicit env: Env): Primitive = e.eval match {
    case Bool(v) => Bool(!v)
    case v       => throw NotSupportedOperatorError(pos, character, v)
  }
}
