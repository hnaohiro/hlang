package hlang

import hlang.element.Primitive
import hlang.element.premitive.{BoolValue, FloatValue, IntValue, StringValue}
import hlang.error.NotSupportedTypeError

class Env(private val variables: Map[String, Primitive]) {
  def get(key: String): Option[Primitive] = variables.get(key)
  override def toString: String = variables.toString
}

object Env {
  def apply(in: Map[String, Any]): Env = {
    new Env(in.map {
      case (key, value: Int)     => (key, IntValue(value))
      case (key, value: Long)    => (key, FloatValue(value))
      case (key, value: Float)   => (key, FloatValue(value))
      case (key, value: Double)  => (key, FloatValue(value))
      case (key, value: Boolean) => (key, BoolValue(value))
      case (key, value: String)  => (key, StringValue(value))
      case (_, value)            => throw NotSupportedTypeError(value.getClass)
    })
  }
}
