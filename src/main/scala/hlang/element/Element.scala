package hlang.element

import scala.util.parsing.input.Positional

trait Element extends Positional {
  def eval: Primitive
}

trait Operator extends Element {
  def character: String
}

trait BinaryOperator extends Operator {
  val e1, e2: Element
}

trait UnaryOperator extends Operator {
  val e: Element
}

trait Primitive extends Element {
  def eval: Primitive = this
}

trait Statement extends Element
