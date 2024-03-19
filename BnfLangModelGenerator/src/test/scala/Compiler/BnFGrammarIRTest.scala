package Compiler

import LexerParser.{Nonterminal, *}
import LiteralType.*
import Utilz.PrologTemplate
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BnFGrammarIRTest extends AnyFlatSpec with Matchers {
  behavior of "manipulations over IR data structures"

  it should "confirm that the rewriting of this prolog fact with program entities only is finished" in {
    val pf = PrologFact("sum_sub", List(("ProductDivRepetition",
      List(RepeatPrologFact(List(
        PrologFact("product_div_repetition", List(("Sign", List(ProgramEntity("+"))),
          ("ProductDiv", List(PrologFact("product_div", List(("_", List()),
            ("NumberOrExpression", List(PrologFact("term", List(("Number", List(ProgramEntity("049.21"))))))),
            ("TermRepetition", List(PrologFact("term_repetition", List(("Sign", List(ProgramEntity("*"))),
              ("Term", List(PrologFact("term", List(("Number", List(ProgramEntity("-78.30"))))))))))))))))),
        PrologFact("product_div_repetition", List(("Sign", List(ProgramEntity("+"))),
          ("ProductDiv", List(PrologFact("product_div", List(("_", List()),
            ("NumberOrExpression", List(PrologFact("term", List(("Number", List(ProgramEntity("8.28"))))))),
            ("TermRepetition", List(PrologFact("term_repetition", List(("Sign", List(ProgramEntity("*"))),
              ("Term", List(PrologFact("term", List(("Number", List(ProgramEntity("1.44"))))))))))))))))),
        PrologFact("product_div_repetition", List(("Sign", List(ProgramEntity("+"))),
          ("ProductDiv", List(PrologFact("product_div", List(("_", List()),
            ("NumberOrExpression", List(PrologFact("term", List(("Number", List(ProgramEntity("+464.33"))))))),
            ("TermRepetition", List(PrologFact("term_repetition", List(("Sign", List(ProgramEntity("*"))),
              ("Term", List(PrologFact("term", List(("Number", List(ProgramEntity("+3"))))))))))))))))))))),
      ("_", List(PrologFact("product_div", List(("_", List()),
        ("NumberOrExpression", List(PrologFact("term", List(("Number", List(ProgramEntity("-33.56"))))))),
        ("TermRepetition", List(PrologFact("term_repetition", List(("Sign", List(ProgramEntity("*"))),
          ("Term", List(PrologFact("term", List(("Number", List(ProgramEntity("-44")))))))))))))))))
    pf.isRewriteCompleted() shouldBe true
  }
}
