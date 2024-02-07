package Generator

import org.scalatest.funsuite.AnyFunSuiteLike
import LexerParser.{Nonterminal, *}
import Compiler.*
import LiteralType.*
import Utilz.{CreateLogger, PrologTemplate}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.slf4j.Logger

class GeneratedProgramStateTest extends AnyFlatSpec with Matchers {
  behavior of "the IR extractors"

  val logger: Logger = CreateLogger(classOf[GeneratedProgramStateTest])
  val expGrammarFull: List[ProductionRule] = List(
    ProductionRule(
      BnfLiteral("expression", NONTERM),
      SeqConstruct(List(
        GroupConstruct(List(
          BnfLiteral("sum_sub", NONTERM),
          PrologFactsBuilder(PrologTemplate("expression", List(PrologTemplate("SumSub", List())))))))
      )
    ),
    ProductionRule(
      BnfLiteral("sum_sub", NONTERM),
      SeqConstruct(List(
        GroupConstruct(List(
          BnfLiteral("product_div", NONTERM),
          RepeatConstruct(List(
            GroupConstruct(List(
              GroupConstruct(List(
                UnionConstruct(List(
                  GroupConstruct(List(BnfLiteral("+", TERM))),
                  GroupConstruct(List(BnfLiteral("-", TERM))))))
              ),
              BnfLiteral("product_div", NONTERM),
              PrologFactsBuilder(PrologTemplate("product_div_repetition", List(PrologTemplate("Sign", List()), PrologTemplate("ProductDiv", List())))))))
          ),
          PrologFactsBuilder(PrologTemplate("sum_sub", List(PrologTemplate("_", List()), PrologTemplate("ProductDivRepetition", List())))))))
      )
    ),
    ProductionRule(
      BnfLiteral("product_div", NONTERM),
      SeqConstruct(List(
        GroupConstruct(List(
          OptionalConstruct(List(
            UnionConstruct(List(
              GroupConstruct(List(
                BnfLiteral("+", TERM))
              ),
              GroupConstruct(List(
                BnfLiteral("-", TERM))
              ))))
          ),
          BnfLiteral("term", NONTERM),
          RepeatConstruct(List(
            GroupConstruct(List(
              GroupConstruct(List(
                UnionConstruct(List(
                  GroupConstruct(List(BnfLiteral("*", TERM))),
                  GroupConstruct(List(BnfLiteral("/", TERM))))))
              ),
              BnfLiteral("term", NONTERM),
              PrologFactsBuilder(PrologTemplate("term_repetition", List(
                PrologTemplate("Sign", List()),
                PrologTemplate("Term", List())))))))
          ),
          MetaVariable("PrevProductDiv", List("sum_sub", "product_div", "_1")),
          PrologFactsBuilder(PrologTemplate("product_div", List(
            PrologTemplate("PrevProductDiv", List()),
            PrologTemplate("_", List()),
            PrologTemplate("NumberOrExpression", List()),
            PrologTemplate("TermRepetition", List()))))))))
    ),
    ProductionRule(
      BnfLiteral("term", NONTERM),
      SeqConstruct(List(
        UnionConstruct(List(
          GroupConstruct(List(
            BnfLiteral("number", NONTERM),
            MetaVariable("PrevTerm", List("product_div", "term", "_2")),
            PrologFactsBuilder(PrologTemplate("term", List(PrologTemplate("PrevTerm", List()), PrologTemplate("Number", List())))))
          ),
          GroupConstruct(List(
            BnfLiteral("(", TERM),
            BnfLiteral("expression", NONTERM),
            BnfLiteral(")", TERM),
            PrologFactsBuilder(PrologTemplate("term", List(
              PrologTemplate("_", List()),
              PrologTemplate("Expression", List()),
              PrologTemplate("_", List()))))))))))
    ),
    ProductionRule(BnfLiteral("number", NTREGEX), BnfLiteral("""[\-\+]?[0-9]{1,3}(\.[0-9]{2})?""", REGEXTERM)))

  it should "extract an IR representation from a union rule with a collection of elements" in {
    val gen = ProgramGenerator(expGrammarFull, BnfLiteral("expression", NONTERM))
    gen match
      case Left(err) =>
        logger.error(s"Cannot generate a program: $err")
        fail()
      case Right(prg) =>
        logger.info(prg.mkString(" "))
        prg shouldBe List()

  }
}
