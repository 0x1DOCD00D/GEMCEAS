enum Ops(val value: String):
  case ADD extends Ops("+")
  case SUB extends Ops("-")
  case MUL extends Ops("*")
  case DIV extends Ops("/")
  