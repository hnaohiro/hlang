package hlang.element

import hlang.Env

import scala.util.parsing.input.Positional

trait Element extends Positional {
  def eval(implicit env: Env): Primitive
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
  def eval(implicit env: Env): Primitive = this
}

trait Statement extends Element
