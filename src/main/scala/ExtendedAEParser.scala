import ArithmeticExpressionParser.floatingPointNumber

import scala.util.parsing.combinator.JavaTokenParsers


object ExtendedAEParser extends JavaTokenParsers:
  import scala.util.parsing.combinator.*

  def expr: Parser[Any] = opt("+"|"-")~term~rep("+"~term | "-"~term)
  def term: Parser[Any] = factor~rep("*"~factor | "/"~factor)
  def factor: Parser[Any] = rep("+"|"-") ~ floatingPointNumber | "("~expr~")"

  @main def runMain_ArithExpParser(): Unit =
    parseAll(ExtendedAEParser.expr, "+(-+13   * (-2-1)/3.12)") match
      case failed: ArithmeticExpressionParser.Failure => println(failed.msg)
      case success => println(success)
