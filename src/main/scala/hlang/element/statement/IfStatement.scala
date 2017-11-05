package hlang.element.statement

import hlang.element.premitive.Bool
import hlang.element.{Element, Primitive, Statement}
import hlang.error.TypeMismatchError

case class IfStatement(cond: Element, ifStatement: Element, elseStatement: Element) extends Statement {

  def eval: Primitive = {
    cond.eval match {
      case Bool(true)  => ifStatement.eval
      case Bool(false) => elseStatement.eval
      case v           => throw TypeMismatchError(pos, v, Bool.getClass)
    }
  }
}
