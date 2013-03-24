package nlpcourse

import nlpcourse._
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class ParsingTest extends FunSuite with ShouldMatchers {
	val S = NT("S")
	val NP = NT("NP")
	val Vt = NT("Vt")
	val VP = NT("VP")
	val DT = NT("DT")
	val IN = NT("IN")
	val PP = NT("PP")
	val NN = NT("NN")
	val CC = NT("CC")
	val saw = T("saw")
	val the = T("the")
	val John = T("John")
	val Mary = T("Mary")
	val Sally = T("Sally")
	val `with` = T("with")
	val dog = T("dog")
	val cat = T("cat")
	val house = T("house")
	val mouse = T("mouse")
	val and = T("and")
	val in = T("IN")

	def secondQuizSixthQuestionCFG: CFG = {
		val cfg = new CFG();
		cfg.rules(
			S -> (NP, VP) withProb 1.0,
			VP -> (Vt, NP) withProb 0.2,
			VP -> (VP, PP) withProb 0.8,
			NP -> (NP, PP) withProb 0.2,
			NP -> (John) withProb 0.16,
			NP -> (Mary) withProb 0.24,
			NP -> (Sally) withProb 0.4,
			PP -> (IN, NP) withProb 1.0,
			IN -> (`with`) withProb 1.0,
			Vt -> (saw) withProb 1.0
		)
		cfg
	}

	def secondQuizSixthSecondQuestionParser(ignoreProbabilities: Boolean): NLPParser = {
		new NLPParser(secondQuizSixthQuestionCFG, ignoreProbabilities)
	}

	def secondQuizSixthSecondQuestionExampleSentence: Sentence = {
		List("John", "saw", "Mary", "with", "Sally")
	}
	def secondQuizSixthQuestionParseResult1: ParseTree = {
		ParseTree(S,
			ParseTree(NP,
				ParseTree(John)
			),
			ParseTree(VP,
				ParseTree(Vt,
					ParseTree(saw)
				),
				ParseTree(NP,
					ParseTree(NP,
						ParseTree(Mary)
					),
					ParseTree(PP,
						ParseTree(IN,
							ParseTree(`with`)
						),
						ParseTree(NP,
							ParseTree(Sally)
						)
					)
				)
			)
		)
	}
	def secondQuizSixthQuestionParseResult2: ParseTree = {
		ParseTree(S,
			ParseTree(NP,
				ParseTree(John)
			),
			ParseTree(VP,
				ParseTree(VP,
					ParseTree(Vt,
						ParseTree(saw)
					),
					ParseTree(NP,
						ParseTree(Mary)
					)
				),
				ParseTree(PP,
					ParseTree(IN,
						ParseTree(`with`)
					),
					ParseTree(NP,
						ParseTree(Sally)
					)
				)
			)
		)
	}

	test("CFG returns correct probability.") {
		val cfg = secondQuizSixthQuestionCFG
		cfg.q(NP -> (John)) should be > (0.0)
		cfg.q(NP -> (saw)) should be (0.0)
		cfg.q(DT -> (Mary)) should be (0.0)
	}

	test("Dynamic programming table is filled correctly.") {
		val parser = secondQuizSixthSecondQuestionParser(true)
		parser.parse(secondQuizSixthSecondQuestionExampleSentence)
		parser.pi(1, 1, NP) should be (1.0)
		parser.pi(1, 1, VP) should be (0.0)
		parser.pi(2, 2, NP) should be (0.0)
		parser.pi(5, 5, NP) should be (1.0)
		parser.pi(4, 5, PP) should be (1.0)
		parser.pi(3, 5, NP) should be (1.0)
		parser.pi(2, 5, VP) should be (1.0)
		parser.pi(1, 5, S) should be (1.0)
	}

	test("Sentence is correctly parsed (return all possible parse trees).") {
		val parser = secondQuizSixthSecondQuestionParser(true)
		val parseResult = parser.parse(secondQuizSixthSecondQuestionExampleSentence)
		parseResult.trees should contain (secondQuizSixthQuestionParseResult1)
		parseResult.trees should contain (secondQuizSixthQuestionParseResult2)
		parseResult.prob should be (1)
	}

	test("Sentence is correctly parsed when using probabilities.") {
		val parser = secondQuizSixthSecondQuestionParser(false)
		val parseResult = parser.parse(secondQuizSixthSecondQuestionExampleSentence)
		parseResult.trees should not contain (secondQuizSixthQuestionParseResult1)
		parseResult.trees should contain (secondQuizSixthQuestionParseResult2)
		parseResult.prob should be (0.0024576 plusOrMinus 0.0000001)
	}
}
