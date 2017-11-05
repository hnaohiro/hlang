package hlang.error

import scala.util.parsing.input.Position

trait RuntimeError extends Error {

  val position: Position
  val message: String

  override def toString: String = {
    s"""
       |Error: $message
       |
       |line: ${position.line}, column: ${position.column}
       |```
       |${position.longString}
       |```
    """.stripMargin
  }
}
