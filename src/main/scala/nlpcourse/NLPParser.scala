package nlpcourse

import nlpcourse._
import scala.collection.mutable.Map

class Symbol(name: String)
// Terminal symbol
case class T(name: String) extends Symbol(name)
// Non-terminal symbol
case class Rule(leftSide: NT, rightSide: Symbol*)

case class NT(name: String) extends Symbol(name) {
	def ->(symbols: Symbol*): Rule = {
		Rule(this, symbols: _*)
	}
}


class CFG {
	var rules: List[Rule] = _
	def rules(value: Rule*) {
		rules = value.toList
	}

	def NT: List[NT] = {
		rules.map(_.leftSide)
	}

	def q(rule: Rule): Double = {
		if (rules.contains(rule))
			1.0
		else
			0.0
	}
}

class ParseTree(s: Symbol, children: Symbol*)

class NLPParser(cfg: CFG) {

	val table = Map[(Int, Int, NT), Double]()

	def parse(sentence: Sentence): List[ParseTree] = {
		val n = sentence.length
		for (i <- (1 to n); X <- cfg.NT)
			table(i, i, X) = cfg.q(X -> T((sentence(i - 1)))) // need to offset -1 because we start at 1

		for (i <- (1 to (n - 1))) {
		}

		List()
	}
}