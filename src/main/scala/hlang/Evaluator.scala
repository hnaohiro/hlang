package hlang

object Evaluator {
  def eval(element: Element): Value = element match {
    case Or(e1, e2)                 => eval(e1) || eval(e2)
    case And(e1, e2)                => eval(e1) && eval(e2)
    case Equal(e1, e2)              => eval(e1) == eval(e2)
    case NotEqual(e1, e2)           => eval(e1) != eval(e2)
    case LowerThanOrEqual(e1, e2)   => eval(e1) <= eval(e2)
    case GreaterThanOrEqual(e1, e2) => eval(e1) >= eval(e2)
    case LowerThan(e1, e2)          => eval(e1) < eval(e2)
    case GreaterThan(e1, e2)        => eval(e1) > eval(e2)
    case Add(e1, e2)                => eval(e1) + eval(e2)
    case Sub(e1, e2)                => eval(e1) - eval(e2)
    case Mul(e1, e2)                => eval(e1) * eval(e2)
    case Div(e1, e2)                => eval(e1) / eval(e2)
    case Not(e)                     => !eval(e)
    case Negative(e)                => -eval(e)
    case Bool(e)                    => Value(BoolType, e)
    case Number(e)                  => Value(NumberType, e)
  }
}
