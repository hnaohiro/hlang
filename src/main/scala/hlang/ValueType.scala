package hlang

sealed abstract class ValueType
case object BoolType extends ValueType
case object NumberType extends ValueType
