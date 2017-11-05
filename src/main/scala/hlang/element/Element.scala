package hlang.element

trait Element {
  def eval: Primitive
}

trait Operator extends Element {
  def character: String
  def expression: String
}

trait BinaryOperator extends Operator {
  val e1, e2: Element
  def expression: String = s"${e1.eval} $character ${e2.eval}"
}

trait UnaryOperator extends Operator {
  val e: Element
  def expression: String = s"$character${e.eval}"
}

trait Primitive extends Element {
  def eval: Primitive = this
  def expression: String = toString
}

trait Statement extends Element
