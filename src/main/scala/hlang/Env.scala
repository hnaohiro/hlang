package hlang

import hlang.element.Primitive
import hlang.element.premitive.{Bool, Number}
import hlang.error.NotSupportedTypeError

class Env(private val variables: Map[String, Primitive]) {
  def get(key: String): Option[Primitive] = variables.get(key)
  override def toString: String = variables.toString
}

object Env {
  def apply(in: Map[String, AnyVal]): Env = {
    new Env(in.map {
      case (key, value: Int)     => (key, Number(value))
      case (key, value: Long)    => (key, Number(value))
      case (key, value: Float)   => (key, Number(value))
      case (key, value: Double)  => (key, Number(value))
      case (key, value: Boolean) => (key, Bool(value))
      case (_, value)            => throw NotSupportedTypeError(value.getClass)
    })
  }
}
