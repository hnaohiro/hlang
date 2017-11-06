package hlang

import hlang.element._
import hlang.element.operator._
import hlang.element.premitive._
import hlang.element.statement.IfStatement
import hlang.element.variable.Variable

import scala.util.control.Exception.allCatch
import scala.util.parsing.combinator.JavaTokenParsers

class Parser extends JavaTokenParsers {

  def expr: Parser[Element] = positioned {
    ifStatement | or
  }

  def ifStatement: Parser[Element] = positioned {
    "if" ~ expr ~ ":" ~ expr ~ "else" ~ ":" ~ expr ^^ {
      case "if" ~ cond ~ ":" ~ ifStatement ~ "else" ~ ":" ~ elseStatement =>
        IfStatement(cond, ifStatement, elseStatement)
    }
  }

  def or: Parser[Element] = positioned {
    and ~ rep("||" ~> and) ^^ {
      case e ~ es => es.foldLeft(e) { case (e1, e2) => Or(e1, e2) }
    }
  }

  def and: Parser[Element] = positioned {
    eq ~ rep("&&" ~> eq) ^^ {
      case e ~ es => es.foldLeft(e) { case (e1, e2) => And(e1, e2) }
    }
  }

  def eq: Parser[Element] = positioned {
    comp ~ rep(("==" | "!=") ~ comp) ^^ {
      case e ~ es =>
        es.foldLeft(e) {
          case (e1, "==" ~ e2) => Equal(e1, e2)
          case (e1, "!=" ~ e2) => NotEqual(e1, e2)
        }
    }
  }

  def comp: Parser[Element] = positioned {
    add ~ rep(("<=" | ">=" | "<" | ">") ~ add) ^^ {
      case e ~ es =>
        es.foldLeft(e) {
          case (e1, "<=" ~ e2) => LowerThanOrEqual(e1, e2)
          case (e1, ">=" ~ e2) => GreaterThanOrEqual(e1, e2)
          case (e1, "<" ~ e2)  => LowerThan(e1, e2)
          case (e1, ">" ~ e2)  => GreaterThan(e1, e2)
        }
    }
  }

  def add: Parser[Element] = positioned {
    mul ~ rep(("+" | "-") ~ mul) ^^ {
      case e ~ es =>
        es.foldLeft(e) {
          case (e1, "+" ~ e2) => Add(e1, e2)
          case (e1, "-" ~ e2) => Sub(e1, e2)
        }
    }
  }

  def mul: Parser[Element] = positioned {
    factor ~ rep(("*" | "/") ~ factor) ^^ {
      case e ~ es =>
        es.foldLeft(e) {
          case (e1, "*" ~ e2) => Mul(e1, e2)
          case (e1, "/" ~ e2) => Div(e1, e2)
        }
    }
  }

  def factor: Parser[Element] = positioned {
    number | bool | str | variable | not | negative | "(" ~> expr <~ ")"
  }

  def number: Parser[Element] = positioned {
    floatingPointNumber ^^ { x =>
      allCatch either x.toInt match {
        case Left(_)  => FloatValue(x.toDouble)
        case Right(v) => IntValue(v)
      }
    }
  }

  def bool: Parser[Element] = positioned {
    ("true" | "false") ^^ (x => BoolValue(x.toBoolean))
  }

  def str: Parser[Element] = positioned {
    "\"" ~> ident <~ "\"" ^^ (x => StringValue(x))
  }

  def variable: Parser[Element] = positioned {
    ident ^^ (x => Variable(x))
  }

  def not: Parser[Element] = positioned {
    "!" ~> factor ^^ (x => Not(x))
  }

  def negative: Parser[Element] = positioned {
    "-" ~> factor ^^ (x => Negative(x))
  }
}

object ParserTest extends App {
  implicit val env: Env = Env(Map("gender" -> "m", "age" -> 28))

  val input =
    """
      |if age >= 20:
      |  if gender != "m":
      |    100
      |  else:
      |    1.1e+2
      |else:
      |  -1
    """.stripMargin

  val parser = new Parser
  val result = parser.parseAll(parser.expr, input)

  println(result)
  println(result.get.eval)
}
