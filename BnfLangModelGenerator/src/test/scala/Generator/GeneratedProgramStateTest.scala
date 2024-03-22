package Generator

import Compiler.*
import Generator.ProgramGenerator.logger
import LiteralType.*
import Utilz.{CreateLogger, PrologTemplate}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.slf4j.Logger

class GeneratedProgramStateTest extends AnyFlatSpec with Matchers {
  behavior of "the IR extractors"

  val logger: Logger = CreateLogger(classOf[GeneratedProgramStateTest])
  val expGrammarFullWithMetaVars: List[ProductionRule] = List(
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
          MetaVariable("PrevProductDiv", List("sum_sub", "product_div"), Some(2)),
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
            MetaVariable("PrevTerm", List("product_div", "term"), Some(2)),
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

  it should "generate an expression for the provided grammar with metavariables" in {
    val gen = ProgramGenerator(expGrammarFullWithMetaVars, BnfLiteral("expression", NONTERM))
    gen match
      case Left(err) => err.isEmpty shouldBe false
      case Right(prg) =>
        logger.info(prg.mkString(" "))
        prg.length should be >= 1
  }
/*
  it should "create an expression given its parsed grammar" in {
    val progstate = GeneratedProgramState(List(ProgramEntity("-8.11"), ProgramEntity("*"), ProgramEntity("-47"),
      RepeatPrologFact(List(
        PrologFact("product_div_repetition", List(("Sign", List(ProgramEntity("+"))),
          ("ProductDiv", List(PrologFact("product_div", List(("_", List()),
            ("NumberOrExpression", List(PrologFact("term", List(("Number", List(ProgramEntity("84.00"))))))),
            ("TermRepetition", List(PrologFact("term_repetition", List(("Sign", List(ProgramEntity("*"))),
              ("Term", List(PrologFact("term", List(("Number", List(ProgramEntity("+93.97"))))))))))))))))),
        PrologFact("product_div_repetition", List(("Sign", List(ProgramEntity("+"))),
          ("ProductDiv", List(PrologFact("product_div", List(("_", List()),
            ("NumberOrExpression", List(PrologFact("term", List(("Number", List(ProgramEntity("50"))))))),
            ("TermRepetition", List(PrologFact("term_repetition", List(("Sign", List(ProgramEntity("*"))),
              ("Term", List(PrologFact("term", List(("Number", List(ProgramEntity("+421"))))))))))))))))),
        PrologFact("product_div_repetition", List(("Sign", List(ProgramEntity("+"))), ("ProductDiv", List(PrologFact("product_div", List(("_", List()),
          ("NumberOrExpression", List(PrologFact("term", List(("Number", List(ProgramEntity("0"))))))),
          ("TermRepetition", List(PrologFact("term_repetition", List(("Sign", List(ProgramEntity("*"))),
            ("Term", List(PrologFact("term", List(("Number", List(ProgramEntity("24"))))))))))))))))
        )
      )
      )
    ))
    val codeBnF = progstate.convertPrologFactsIntoBnFElements()
    val code = ProgramGenerator.forTesting(codeBnF)
    logger.info(s"Generated program: ${code.mkString(" ")}")
    code.mkString(" ").isBlank shouldBe false//("+ 84.00 * +93.97 + 50 * +421 + 0 * 24")
  }
*/
}
