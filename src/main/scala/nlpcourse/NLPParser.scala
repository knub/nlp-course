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

	def rulesFor(X: NT): List[Rule] = {
		rules.filter { rule =>
			rule.leftSide == X && rule.rightSide.size == 2
		}
	}
}

class ParseTree(s: Symbol, children: Symbol*)

case class ParseTmp(rule: Rule, s: Int, prob: Double)
case class ParseResult(trees: List[ParseTree], prob: Double)
class NLPParser(cfg: CFG) {

	val pi = Map[(Int, Int, NT), Double]()
	val bp = Map[(Int, Int, NT), List[ParseTmp]]()

	def parse(sentence: Sentence): ParseResult = {
		val n = sentence.length
		for (i <- (1 to n); X <- cfg.NT)
			pi(i, i, X) = cfg.q(X -> T((sentence(i - 1)))) // need to offset -1 because we start at 1

		for (l <- (1 to (n - 1))) {
			for (i <- (1 to (n - l))) {
				val j = i + l
				for (X <- cfg.NT) {
					val parseTmps = for (r <- cfg.rulesFor(X); s <- (i to (j - 1))) yield {
						val Y = r.rightSide(0).asInstanceOf[NT]
						val Z = r.rightSide(1).asInstanceOf[NT]
						val prob = cfg.q(r) * pi((i, s, Y)) * pi((s + 1, j, Z));
						ParseTmp(r, s, prob)
					}
					if (parseTmps.isEmpty) {
						pi(i, j, X) = 0.0
						bp(i, j, X) = List()
					}
					else {
						val maxProb = parseTmps.map { tmp => tmp.prob }.max
						val maxTrees = parseTmps.filter { tmp => tmp.prob == maxProb }
						pi(i, j, X) = maxProb
						bp(i, j, X) = maxTrees
					}
				}
			}
		}

		ParseResult(List(), pi(1, n, NT("S")))
	}
}