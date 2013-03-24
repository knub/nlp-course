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
	val `with` = T("with")
	val dog = T("dog")
	val cat = T("cat")
	val house = T("house")
	val mouse = T("mouse")
	val and = T("and")
	val in = T("IN")

	def secondQuizSecondQuestionCFG: CFG = {
		val cfg = new CFG();
		cfg.rules(
			S -> (NP, VP),
			VP -> (Vt, NP),
			Vt -> (saw),
			NP -> (John),
			NP -> (DT, NN),
			DT -> (the),
			NN -> (dog),
			NN -> (cat),
			NN -> (house),
			NN -> (mouse),
			NP -> (NP, CC, NP),
			CC -> (and),
			PP -> (IN, NP),
			NP -> (NP, PP),
			IN -> (`with`),
			IN -> (in)
		)
		cfg
	}

	def secondQuizSecondQuestionParser: NLPParser = {
		new NLPParser(secondQuizSecondQuestionCFG)
	}

	def secondQuizSecondQuestionExampleSentence: Sentence = {
		List("John", "saw", "the", "cat", "and", "the", "dog", "with", "the", "mouse")
	}

	test("CFG returns correct probability.") {
		val cfg = secondQuizSecondQuestionCFG
		cfg.q(DT -> (the)) should be (1.0)
		cfg.q(DT -> (cat)) should be (0.0)
	}

	test("Dynamic programming table is filled correctly.") {
		val parser = secondQuizSecondQuestionParser
		parser.parse(secondQuizSecondQuestionExampleSentence)
		parser.table(1, 1, NP) should be (1.0)
		parser.table(2, 2, NP) should be (0.0)
	}

	test("Sentence is correctly parsed (return all possible parse trees).") {
		val parser = secondQuizSecondQuestionParser
		val parseResult = parser.parse(secondQuizSecondQuestionExampleSentence)
		// println(parseResult)
		// parseResult.size should be (2)
	}
}
