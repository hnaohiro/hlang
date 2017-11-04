package hlang

case class Value(interfaceType: ValueType, value: Any) {

  def ||(other: Value): Value = {
    val newValue = value.asInstanceOf[Boolean] || other.value.asInstanceOf[Boolean]
    Value(interfaceType, newValue)
  }

  def &&(other: Value): Value = {
    val newValue = value.asInstanceOf[Boolean] && other.value.asInstanceOf[Boolean]
    Value(interfaceType, newValue)
  }

  def ==(other: Value): Value = {
    val newValue = (interfaceType, other.interfaceType) match {
      case (BoolType, BoolType)     => value.asInstanceOf[Boolean] == other.value.asInstanceOf[Boolean]
      case (NumberType, NumberType) => value.asInstanceOf[Double] == other.value.asInstanceOf[Double]
      case _                        => false
    }
    Value(BoolType, newValue)
  }

  def !=(other: Value): Value = {
    val newValue = value.asInstanceOf[Boolean] != other.value.asInstanceOf[Boolean]
    Value(BoolType, newValue)
  }

  def <=(other: Value): Value = {
    val newValue = value.asInstanceOf[Double] <= other.value.asInstanceOf[Double]
    Value(BoolType, newValue)
  }

  def >=(other: Value): Value = {
    val newValue = value.asInstanceOf[Double] >= other.value.asInstanceOf[Double]
    Value(BoolType, newValue)
  }

  def <(other: Value): Value = {
    val newValue = value.asInstanceOf[Double] < other.value.asInstanceOf[Double]
    Value(BoolType, newValue)
  }

  def >(other: Value): Value = {
    val newValue = value.asInstanceOf[Double] > other.value.asInstanceOf[Double]
    Value(BoolType, newValue)
  }

  def +(other: Value): Value = {
    val newValue = value.asInstanceOf[Double] + other.value.asInstanceOf[Double]
    Value(NumberType, newValue)
  }

  def -(other: Value): Value = {
    val newValue = value.asInstanceOf[Double] - other.value.asInstanceOf[Double]
    Value(NumberType, newValue)
  }

  def *(other: Value): Value = {
    val newValue = value.asInstanceOf[Double] * other.value.asInstanceOf[Double]
    Value(NumberType, newValue)
  }

  def /(other: Value): Value = {
    val newValue = value.asInstanceOf[Double] / other.value.asInstanceOf[Double]
    Value(NumberType, newValue)
  }

  def unary_-(): Value = {
    val newValue = -1 * value.asInstanceOf[Double]
    Value(NumberType, newValue)
  }

  def unary_!(): Value = {
    val newValue = !value.asInstanceOf[Boolean]
    Value(BoolType, newValue)
  }
}
