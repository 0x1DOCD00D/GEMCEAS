case class ArithExp[T](exp1: T, op: Ops, exp2: T) {
  override def toString: String =
    exp1.toString + op.value + exp2.toString
}


