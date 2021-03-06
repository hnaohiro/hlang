package hlang.element.statement

import hlang.Env
import hlang.element.premitive.BoolValue
import hlang.element.{Element, Primitive, Statement}
import hlang.error.TypeMismatchError

case class IfStatement(cond: Element, ifStatement: Element, elseStatement: Element) extends Statement {

  def eval(implicit env: Env): Primitive = {
    cond.eval match {
      case BoolValue(true)  => ifStatement.eval
      case BoolValue(false) => elseStatement.eval
      case v                => throw TypeMismatchError(pos, v, BoolValue.getClass)
    }
  }
}
