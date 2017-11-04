package hlang

import scala.util.parsing.combinator.JavaTokenParsers

class ConditionParser extends JavaTokenParsers {

  def expr: Parser[Element] = term ~ rep("||" ~ term) ^^ {
    case e ~ es =>
      es.foldLeft(e) {
        case (e1, "||" ~ e2) => Or(e1, e2)
      }
  }

  def term: Parser[Element] = eq ~ rep("&&" ~ eq) ^^ {
    case e ~ es =>
      es.foldLeft(e) {
        case (e1, "&&" ~ e2) => And(e1, e2)
      }
  }

  def eq: Parser[Element] = comp ~ rep(("==" | "!=") ~ comp) ^^ {
    case e ~ es =>
      es.foldLeft(e) {
        case (e1, "==" ~ e2) => Equal(e1, e2)
        case (e1, "!=" ~ e2) => NotEqual(e1, e2)
      }
  }

  def comp: Parser[Element] = addsub ~ rep(("<=" | ">=" | "<" | ">") ~ addsub) ^^ {
    case e ~ es =>
      es.foldLeft(e) {
        case (e1, "<=" ~ e2) => LowerThanOrEqual(e1, e2)
        case (e1, ">=" ~ e2) => GreaterThanOrEqual(e1, e2)
        case (e1, "<" ~ e2)  => LowerThan(e1, e2)
        case (e1, ">" ~ e2)  => GreaterThan(e1, e2)
      }
  }

  def addsub: Parser[Element] = muldiv ~ rep(("+" | "-") ~ muldiv) ^^ {
    case e ~ es =>
      es.foldLeft(e) {
        case (e1, "+" ~ e2) => Add(e1, e2)
        case (e1, "-" ~ e2) => Sub(e1, e2)
      }
  }

  def muldiv: Parser[Element] = factor ~ rep(("*" | "/") ~ factor) ^^ {
    case e ~ es =>
      es.foldLeft(e) {
        case (e1, "*" ~ e2) => Mul(e1, e2)
        case (e1, "/" ~ e2) => Div(e1, e2)
      }
  }

  def factor: Parser[Element] = number | bool | not | negative | "(" ~> expr <~ ")"

  def number: Parser[Number] = floatingPointNumber ^^ (x => Number(x.toDouble))

  def bool: Parser[Bool] = ("true" | "false") ^^ (x => Bool(x.toBoolean))

  def not: Parser[Element] = "!" ~> factor ^^ (x => Not(x))

  def negative: Parser[Element] = "-" ~> factor ^^ (x => Negative(x))
}

object ConditionParserTest extends App {
  val input = "-(100 * 2)"
  val parser = new ConditionParser
  val result = parser.parseAll(parser.expr, input)

  println(result)
  println(Evaluator.eval(result.get))
}
