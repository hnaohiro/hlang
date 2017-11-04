package hlang

sealed abstract class Element

case class Or(e1: Element, e2: Element) extends Element
case class And(e1: Element, e2: Element) extends Element
case class Equal(e1: Element, e2: Element) extends Element
case class NotEqual(e1: Element, e2: Element) extends Element
case class LowerThanOrEqual(e1: Element, e2: Element) extends Element
case class GreaterThanOrEqual(e1: Element, e2: Element) extends Element
case class LowerThan(e1: Element, e2: Element) extends Element
case class GreaterThan(e1: Element, e2: Element) extends Element
case class Add(e1: Element, e2: Element) extends Element
case class Sub(e1: Element, e2: Element) extends Element
case class Mul(e1: Element, e2: Element) extends Element
case class Div(e1: Element, e2: Element) extends Element
case class Not(e: Element) extends Element
case class Negative(e: Element) extends Element
case class Number(value: Double) extends Element
case class Bool(value: Boolean) extends Element
