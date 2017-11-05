package hlang.element.variable

import hlang.Env
import hlang.element.Primitive

case class Variable(name: String) extends Primitive {

  override def eval(implicit env: Env): Primitive = {
    env.get(name) match {
      case Some(v) => v.eval
      case None    => throw new Error(s"variable $name is not found")
    }
  }
}
