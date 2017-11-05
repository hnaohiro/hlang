package hlang

import hlang.element._
import hlang.element.operator._
import hlang.element.premitive._
import hlang.element.statement.IfStatement

import scala.util.parsing.combinator.JavaTokenParsers

class Parser extends JavaTokenParsers {

  def expr: Parser[Element] = or | ifStatement

  def ifStatement: Parser[Element] = "if" ~ expr ~ ":" ~ expr ~ "else" ~ ":" ~ expr ^^ {
    case "if" ~ cond ~ ":" ~ ifStatement ~ "else" ~ ":" ~ elseStatement => IfStatement(cond, ifStatement, elseStatement)
  }

  def or: Parser[Element] = and ~ rep("||" ~> and) ^^ {
    case e ~ es => es.foldLeft(e) { case (e1, e2) => Or(e1, e2) }
  }

  def and: Parser[Element] = eq ~ rep("&&" ~> eq) ^^ {
    case e ~ es => es.foldLeft(e) { case (e1, e2) => And(e1, e2) }
  }

  def eq: Parser[Element] = comp ~ rep(("==" | "!=") ~ comp) ^^ {
    case e ~ es =>
      es.foldLeft(e) {
        case (e1, "==" ~ e2) => Equal(e1, e2)
        case (e1, "!=" ~ e2) => NotEqual(e1, e2)
      }
  }

  def comp: Parser[Element] = add ~ rep(("<=" | ">=" | "<" | ">") ~ add) ^^ {
    case e ~ es =>
      es.foldLeft(e) {
        case (e1, "<=" ~ e2) => LowerThanOrEqual(e1, e2)
        case (e1, ">=" ~ e2) => GreaterThanOrEqual(e1, e2)
        case (e1, "<" ~ e2)  => LowerThan(e1, e2)
        case (e1, ">" ~ e2)  => GreaterThan(e1, e2)
      }
  }

  def add: Parser[Element] = mul ~ rep(("+" | "-") ~ mul) ^^ {
    case e ~ es =>
      es.foldLeft(e) {
        case (e1, "+" ~ e2) => Add(e1, e2)
        case (e1, "-" ~ e2) => Sub(e1, e2)
      }
  }

  def mul: Parser[Element] = factor ~ rep(("*" | "/") ~ factor) ^^ {
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

object ParserTest extends App {
  val input =
    """
      |if 10:
      |  10 + 5
      |else:
      |  20
    """.stripMargin

  val parser = new Parser
  val result = parser.parseAll(parser.expr, input)

  println(result)
  println(result.get.eval)
}
