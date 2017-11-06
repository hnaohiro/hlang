package hlang.element.operator

import hlang.Env
import hlang.element.premitive.{Number, Str}
import hlang.element.{BinaryOperator, Element, Primitive}
import hlang.error.{NotSupportedOperatorError, TypeMismatchError}

case class Add(e1: Element, e2: Element) extends BinaryOperator {

  val character = "+"

  def eval(implicit env: Env): Primitive = (e1.eval, e2.eval) match {
    case (Number(v1), Number(v2)) => Number(v1 + v2)
    case (Str(v1), Str(v2))       => Str(v1 + v2)
    case (Number(_), v2)          => throw TypeMismatchError(v2.pos, v2, Number.getClass)
    case (v1, _)                  => throw NotSupportedOperatorError(pos, character, v1)
  }
}
