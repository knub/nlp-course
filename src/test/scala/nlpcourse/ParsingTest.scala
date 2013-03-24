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
			S -> (NP, VP),
			VP -> (Vt, NP),
			VP -> (VP, PP),
			NP -> (NP, PP),
			NP -> (John),
			NP -> (Mary),
			NP -> (Sally),
			PP -> (IN, NP),
			IN -> (`with`),
			Vt -> (saw)
		)
		cfg
	}

	def secondQuizSixthSecondQuestionParser: NLPParser = {
		new NLPParser(secondQuizSixthQuestionCFG)
	}

	def secondQuizSixthSecondQuestionExampleSentence: Sentence = {
		List("John", "saw", "Mary", "with", "Sally")
	}

	test("CFG returns correct probability.") {
		val cfg = secondQuizSixthQuestionCFG
		cfg.q(NP -> (John)) should be (1.0)
		cfg.q(NP -> (saw)) should be (0.0)
		cfg.q(DT -> (Mary)) should be (0.0)
	}

	test("Dynamic programming table is filled correctly.") {
		val parser = secondQuizSixthSecondQuestionParser
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
		val parser = secondQuizSixthSecondQuestionParser
		val parseResult = parser.parse(secondQuizSixthSecondQuestionExampleSentence)
		val parseResult1 =
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
		println(parseResult1)
		println(parseResult.trees(0))
		parseResult.trees should contain (parseResult1)
		parseResult.prob should be (1)
	}
}
