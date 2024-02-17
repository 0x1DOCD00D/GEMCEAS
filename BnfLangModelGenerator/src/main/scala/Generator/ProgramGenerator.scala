/*
 Copyright (c) 7/30/23, 2:26 PM by Mark Grechanik (drmark) and Lone Star Consulting, Inc. All rights reserved.

 Licensed under the Apache License, Version 2.0 (the “License”); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 */

package Generator

import Compiler.LiteralType.TERM
import Compiler.*
import Utilz.ConfigDb.{`Gemceas.Generator.debugProgramGeneration`, `Gemceas.Generator.grammarMaxDepthRewriting`}
import Utilz.{ConfigDb, CreateLogger, PrologTemplate}

import java.util.UUID
import scala.annotation.tailrec

class ProgramGenerator private (progGenState: GeneratedProgramState) extends DeriveConstructs:
  private val logger = CreateLogger(classOf[ProgramGenerator])
  @tailrec
  private def generateProgramInstance(prState: GeneratedProgramState = progGenState, level: Int = 0): GeneratedProgramState =
    if !prState.elements.forall(_.isInstanceOf[ProgramEntity]) then
      val accum: List[BnFGrammarIR] = deriveProgram(List(), prState.elements, level)
      generateProgramInstance(GeneratedProgramState(accum), level+1)
    else prState

  @tailrec
  private def deriveProgram(accumulatorProg: List[BnFGrammarIR], st: List[BnFGrammarIR], level: Int, attempt: Int = 1): List[BnFGrammarIR] =
    import DerivationTree.MainRewritingTree
    st match
      case ::(head, next) if head.isInstanceOf[PrologFact] =>
        DerivationTree.resetPrologFact()
        verifyGeneratedProgramFragment(head.asInstanceOf[PrologFact], level) match
          case Some(lst) =>
            deriveProgram(accumulatorProg ::: lst, next, level)
          case None =>
            logger.warn(s"Attempt $attempt failed to obtained a verified derivation of the program fragment $head at level $level")
            deriveProgram(accumulatorProg, next, level, attempt+1)
      case ::(head, next) =>
        val gels = deriveElement(head, level > `Gemceas.Generator.grammarMaxDepthRewriting`)
        DerivationTree.addGrammarElements(gels, head, 0)
        deriveProgram(accumulatorProg ::: gels, next, level)
      case Nil => accumulatorProg
  end deriveProgram

  /*
    The number of args in a template should be equal to the number of top level terminals + nonterminals + repetition/optional constructs (if any). For example, these rules
    expression ::=
      sum_sub
      "==>> expression(SumSub)";
      sum_sub ::=
      product_div {("+"|"-") product_div "==>> product_div_repetition(Sign, ProductDiv)"}
      "==>> sum_sub(_, ProductDivRepetition)";

    illustrate how prolog templetization works. The first rule has one top level construct, the non terminal sum_sub with a prolog template expression(SumSub). It means that the non terminal sum_sub is replaced with the functor expression(SumSub) in the prolog template where SumSub is the param of the functor that contains all the elements of the non terminal sum_sub.

    The second rule defines the nonterminal sum_sub as a production rule that is a sequence of the nonterminal product_div and a repetition construct{("+"|"-") product_div "==>> product_div_repetition(Sign, ProductDiv)"}. The repetition construct is a sequence of the union construct ("+"|"-") and the nonterminal product_div. The repetition construct is replaced with the functor product_div_repetition(Sign, ProductDiv) in the prolog template where Sign is the param of the functor that contains all the elements of the union construct ("+"|"-") and ProductDiv is the param of the functor that contains all the elements of the nonterminal product_div. Whatever code generator produces for (non) terminals of the repetition construct, it should be wrapped in the functor product_div_repetition(Sign, ProductDiv) in the prolog template where the variable, Sign represents a generated sign terminal and the variable, ProductDiv represents a generated nonterminal product_div, recursively.

    Finally, the prolog rule for nonterminal sum_sub is defined as sum_sub(_, ProductDivRepetition) where the variable, ProductDivRepetition represents the functor product_div_repetition(Sign, ProductDiv) that is the result of the prolog template processing.

    When processing a production rule with a prolog template the input parameters to the processor's apply method are the list of elements from the rhs of the production rule and an instance of PrologFactsBuilder that contains the prolog template. The processor's apply method returns the list of elements that are the result of the prolog template processing, which is accomplished by the recursive rewrite of the elements of the rule.

    Suppose we have the following grammar.
    nt ::= nt1 "==>>f(parm)"
    nt1 :: "a" "b" "==>>g(p1, p2)
    The result of this gemceas run is the following prolog template instance f([g("a", "b")]). Also, the generator submits this instance as a whole, not two separate instances of g, first and then, f.  Let’s modify this grammar a bit.

    nt ::= nt1 "==>>f(parm)"
    nt1 :: "a" "b"
    The result of this gemceas run is the following prolog template instance f(["a", "b"]).

    Finally, this grammar has both cases.
    nt ::= nt1 "==>>f(parm)"
    nt1 :: "a" {"b" "==>>g(p)"}
    The result of this gemceas run is the following prolog template instance f(["a", [g("b")]])
  * */

  private def verifyGeneratedProgramFragment(pf: PrologFact, level: Int): Option[List[BnFGrammarIR]] =
    //TODO: a prolog fact is transmitted to the KBLS here and true/false is returned
    def askPrologEngine2Verify(fact: String): Boolean =
      if `Gemceas.Generator.debugProgramGeneration` then logger.info(s"Verified a rewritten prolog fact $fact")
      true

    if pf.isRewriteCompleted() then Some(pf.formListOfBnFGrammarElements)
    else
      if `Gemceas.Generator.debugProgramGeneration` then logger.info(s"Verifying the prolog fact $pf at level $level")
      pf.rewriteGrammarElements(level) match
        case Some(fact) =>
          if askPrologEngine2Verify(fact.generatePrologFact4KBLS(true)) then
            val programInTokens = fact.formListOfBnFGrammarElements
            if `Gemceas.Generator.debugProgramGeneration` then logger.info(s"Formed a verified program fragment $programInTokens")
            Some(programInTokens)
          else None
        case None =>
          logger.error(s"Cannot rewrite the prolog fact $pf at level $level")
          None
  end verifyGeneratedProgramFragment

  private def generateSourceCode(st: GeneratedProgramState): GeneratedProgram =
    if !st.elements.forall(_.isInstanceOf[ProgramEntity]) then
      logger.error(s"The generated program is not well structured.")
      List()
    else
      val code = st.elements.asInstanceOf[List[ProgramEntity]].flatMap(e => List(e.code))
      code
end ProgramGenerator

object ProgramGenerator:
  private val logger = CreateLogger(classOf[ProgramGenerator.type])
  private var _grammar: List[ProductionRule] = _
  private val reachabilityMap: Map[UUID, TerminationData] = Map()
  val expandNT: BnfLiteral => Option[ProductionRule] =
    (nt: BnfLiteral) =>
      if nt.literalType == LiteralType.TERM || nt.literalType == LiteralType.REGEXTERM then
        logger.error(s"BnF literal $nt cannot be used to define a rule.")
        None
      else _grammar.find(_.lhs.asInstanceOf[BnfLiteral].token == nt.token)

  def lookup(go: UUID): Option[TerminationData] = reachabilityMap.get(go)

  def forTesting(codeBnF: GeneratedProgramState): GeneratedProgram =
    new ProgramGenerator(codeBnF).generateSourceCode(codeBnF)

  def apply(g: List[ProductionRule], startRuleId: BnfLiteral): Either[String,GeneratedProgram] =
    if g.length <= 0 then Left("Cannot derive a program using an empty grammar")
    else
      _grammar = g
      val gr = new GrammarRewriter(g)
      val divergentNTs: List[BnFGrammarIR] = gr.grammarConvergenceChecker()
      if divergentNTs.isEmpty then logger.info("The grammar is convergent")
      else
        logger.error("The grammar is divergent")
        divergentNTs.foreach {
          nt => logger.error(s"Divergent NT: ${nt.uuid} -> ${nt.toString}")
        }

      if ConfigDb.`Gemceas.Generator.debugProgramGeneration` then logger.info(s"ProgramGenerator obtains the following reachability map: \n\n${reachabilityMap.mkString("\n\n")}")
      DerivationTree.resetAll()
      DerivationTree.setTheRoot(startRuleId) match
        case Left(err) =>
          logger.error(err)
          Left(err)
        case Right(theRoot) =>
          logger.info(s"Rewriting tree root is set: $theRoot")
          val initState = GeneratedProgramState(List(startRuleId))
          val gen = new ProgramGenerator(initState)
          val programObject: GeneratedProgramState = gen.generateProgramInstance()
          val code: GeneratedProgram = gen.generateSourceCode(programObject)
          Right(code)

  @main def runProgGenWithTemplates(): Unit = {
    /*
    expression ::=
      sum_sub
      "==>> expression(SumSub)";

    sum_sub ::=
      product_div {("+"|"-") product_div "==>> product_div_repetition(Sign, ProductDiv)"}
      "==>> sum_sub(_, ProductDivRepetition)";

    product_div ::=
      ["+"|"-"] term {("*"|"/") term "==>> term_repetition(Sign, Term)"}
      "==>> product_div(_, NumberOrExpression, TermRepetition)";

    term ::=
      number
      "==>> term(Number)" |
      "(" expression ")"
      "==>> term(_, Expression, _)";

    <number> ::=
      "(\+|\-)?[0-9]+(\.[0-9]+)?";
    */
    import Compiler.LiteralType.*
    ConfigDb().foreach((k,v) => logger.info(s"Config key $k => $v"))
    val grammar = List(
      /*
        expression ::=
          sum_sub
          "==>> expression(SumSub)";
      * */
      ProductionRule(BnfLiteral("expression", NONTERM), SeqConstruct(List(
        GroupConstruct(List(
          BnfLiteral("sum_sub", NONTERM),
          PrologFactsBuilder(PrologTemplate("expression", List(PrologTemplate("SumSub", List())))))
        )))
      ),
      /*
        sum_sub ::=
        product_div {("+"|"-") product_div "==>> product_div_repetition(Sign, ProductDiv)"}
        "==>> sum_sub(_, ProductDivRepetition)";
      * */
      ProductionRule(BnfLiteral("sum_sub", NONTERM), SeqConstruct(List(
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
              PrologFactsBuilder(PrologTemplate("product_div_repetition", List(
                PrologTemplate("Sign", List()), PrologTemplate("ProductDiv", List())))
              ))))
          ),
          PrologFactsBuilder(PrologTemplate("sum_sub", List(
            PrologTemplate("_", List()),
            PrologTemplate("ProductDivRepetition", List())))
          )))))
      ),
      /*
        product_div ::=
          ["+"|"-"] term {("*"|"/") term "==>> term_repetition(Sign, Term)"}
          "==>> product_div(_, NumberOrExpression, TermRepetition)";
      * */
      ProductionRule(BnfLiteral("product_div", NONTERM), SeqConstruct(List(
        GroupConstruct(List(
          OptionalConstruct(List(
            UnionConstruct(List(
              GroupConstruct(List(BnfLiteral("+", TERM))),
              GroupConstruct(List(BnfLiteral("-", TERM))))
            ))
          ),
          BnfLiteral("term", NONTERM),
          RepeatConstruct(List(
            GroupConstruct(List(
              GroupConstruct(List(
                UnionConstruct(List(
                  GroupConstruct(List(BnfLiteral("*", TERM))),
                  GroupConstruct(List(BnfLiteral("/", TERM))))
                ))
              ),
              BnfLiteral("term", NONTERM),
              PrologFactsBuilder(PrologTemplate("term_repetition", List(
                PrologTemplate("Sign", List()),
                PrologTemplate("Term", List())))
              ))))
          ),
          PrologFactsBuilder(PrologTemplate("product_div", List(
            PrologTemplate("_", List()),
            PrologTemplate("NumberOrExpression", List()),
            PrologTemplate("TermRepetition", List())))
          )))))
      ),
      /*
        term ::=
          number
          "==>> term(Number)" |
          "(" expression ")"
          "==>> term(_, Expression, _)";
      * */
      ProductionRule(BnfLiteral("term", NONTERM), SeqConstruct(List(
        UnionConstruct(List(
          GroupConstruct(List(
            BnfLiteral("number", NONTERM),
            PrologFactsBuilder(PrologTemplate("term", List(
              PrologTemplate("Number", List())))
            ))
          ),
          GroupConstruct(List(
            BnfLiteral("(", TERM),
            BnfLiteral("expression", NONTERM),
            BnfLiteral(")", TERM),
            PrologFactsBuilder(PrologTemplate("term", List(
              PrologTemplate("_", List()),
              PrologTemplate("Expression", List()),
              PrologTemplate("_", List())))
            )))))))
      ),
      /*
        <number> ::=
          "(\+|\-)?[0-9]+(\.[0-9]+)?";
      * */
      ProductionRule(
        BnfLiteral("number", NTREGEX),
        BnfLiteral("""[\-\+]?[0-9]{1,3}(\.[0-9]{2})?""", REGEXTERM)
      )
    )
    val gen = ProgramGenerator(grammar, BnfLiteral("expression", NONTERM))
    logger.info(s"${gen.toString}")
    gen match
      case Left(err) => logger.error(s"Cannot generate a program: $err")
      case Right(prg) => logger.info(prg.mkString(" "))
  }
  @main def runMain_ProgramGeneratorNoPrologTemplates(): Unit =
    import Compiler.LiteralType.*
    /*
    expression ::= sum_sub;
    sum_sub ::= product_div {("+"|"-") product_div};
    product_div ::= ["+"|"-"] term {("*"|"/") term};
    term ::= number | "(" expression ")";
    <number> ::= "(\+|\-)?[0-9]+(\.[0-9]+)?";
    */
    val grammar = List(
      ProductionRule(BnfLiteral("expression", NONTERM),
        SeqConstruct(List(
          GroupConstruct(List(
            BnfLiteral("sum_sub", NONTERM))
          ))
        )
      ),
      ProductionRule(BnfLiteral("sum_sub", NONTERM),
        SeqConstruct(List(
          GroupConstruct(List(
            BnfLiteral("product_div", NONTERM),
            RepeatConstruct(List(
              GroupConstruct(List(
                GroupConstruct(List(
                  UnionConstruct(List(
                    GroupConstruct(List(
                      BnfLiteral("+", TERM))
                    ),
                    GroupConstruct(List(
                      BnfLiteral("-", TERM))
                    ))
                  ))
                ),
                BnfLiteral("product_div", NONTERM))
              ))
            ))
          ))
        )
      ),
      /*
      product_div ::= ["+"|"-"] term {("*"|"/") term};
      term ::= number | "(" expression ")";
      <number> ::= "(\+|\-)?[0-9]+(\.[0-9]+)?";
      */
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
                ))
              ))
            ),
            BnfLiteral("term", NONTERM),
            RepeatConstruct(List(
              GroupConstruct(List(
                GroupConstruct(List(
                  UnionConstruct(List(
                    GroupConstruct(List(
                      BnfLiteral("*", TERM))
                    ),
                    GroupConstruct(List(
                      BnfLiteral("/", TERM))
                    ))
                  ))
                ),
                BnfLiteral("term", NONTERM))
              ))
            ))
          ))
        )
      ),
      /*
      term ::= number | "(" expression ")";
      <number> ::= "(\+|\-)?[0-9]+(\.[0-9]+)?";
      */
      ProductionRule(
        BnfLiteral("term", NONTERM),
        SeqConstruct(List(
          UnionConstruct(List(
            GroupConstruct(List(
              BnfLiteral("number", NONTERM))
            ),
            GroupConstruct(List(
              BnfLiteral("(", TERM),
              BnfLiteral("expression", NONTERM),
              BnfLiteral(")", TERM))
            ))
          ))
        )
      ),
      ProductionRule(
        BnfLiteral("number", NTREGEX),
        BnfLiteral(""""[-+]?[0-9]+(\\.[0-9]+)?"""", REGEXTERM)
      )
    )

    val gen = ProgramGenerator(grammar, BnfLiteral("expression", NONTERM))
    logger.info(s"${gen.toString}")
end ProgramGenerator
