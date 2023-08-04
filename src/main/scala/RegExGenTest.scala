import Compiler.LiteralType.{REGEXTERM, TERM}
import Compiler.{BnfLiteral, ProgramEntity}

object RegExGenTest:
  import org.scalacheck.*
  import wolfendale.scalacheck.regexp.RegexpGen

  @main def runMain_RegExGenTest(): Unit =
    val generator: Gen[String] = RegexpGen.from("[+|-]?[0-9]+(\\.[0-9]+)?")
    println(generator.sample)