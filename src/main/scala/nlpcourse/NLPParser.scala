package nlpcourse

import nlpcourse._
import scala.collection.mutable.Map

class Symbol(val name: String)
// Terminal symbol
case class T(override val name: String) extends Symbol(name)
// Non-terminal symbol
case class NT(override val name: String) extends Symbol(name) {
	def ->(symbols: Symbol*): Rule = {
		Rule(this, symbols: _*)
	}
}

case class Rule(leftSide: NT, rightSide: Symbol*) {
	var prob: Double = 1.0

	def isNonTerminalRule: Boolean = {
		rightSide.size == 2
	}

	def withProb(value: Double) = {
		prob = value
		this
	}
}


class CFG {
	var rules: List[Rule] = _
	def rules(value: Rule*) {
		rules = value.toList
	}

	def NTs: List[NT] = {
		rules.map(_.leftSide)
	}

	def q(rule: Rule): Double = {
		if (rules.contains(rule))
			rules.find(_ == rule).get.prob
		else
			0.0
	}

	def rulesFor(X: NT): List[Rule] = {
		rules.filter { rule =>
			rule.leftSide == X && rule.isNonTerminalRule
		}
	}

	def startSymbol: NT = {
		NT("S")
	}
}

case class ParseTree(var s: Symbol, children: ParseTree*) {
	override def toString: String = {
		children.size match {
			case 0 => {
				"[\"%s\"]".format(s.name)
			}
			case 1 => {
				"[\"%s\", \"%s\"]".format(s.name, children(0).s.name)
			}
			case 2 => {
				"[\"%s\", %s, %s]".format(s.name, children(0).toString, children(1).toString)
			}
		}
	}
}

case class ParseTmp(rule: Rule, s: Int, prob: Double)
case class ParseResult(trees: List[ParseTree], prob: Double)
class NLPParser(cfg: CFG, ignoreProbabilities: Boolean = false) {

	val pi = Map[(Int, Int, NT), Double]()
	val bp = Map[(Int, Int, NT), List[ParseTmp]]()

	def ruleProbability(rule: Rule) = {
		val prob = cfg.q(rule)
		if (ignoreProbabilities)
			if (prob > 0) 1.0 else 0.0
		else
			prob
	}

	def parse(sentence: Sentence): ParseResult = {
		val n = sentence.length
		for (i <- (1 to n); X <- cfg.NTs) {
			val rule = X -> T(sentence(i - 1)) // need to offset -1 because we start at 1
			pi(i, i, X) = ruleProbability(rule)
			bp(i, i, X) = List(ParseTmp(rule, 0, 1))
		}

		for (l <- (1 to (n - 1))) {
			for (i <- (1 to (n - l))) {
				val j = i + l
				for (X <- cfg.NTs) {
					val parseTmps = for (r <- cfg.rulesFor(X); s <- (i to (j - 1))) yield {
						val Y = r.rightSide(0).asInstanceOf[NT]
						val Z = r.rightSide(1).asInstanceOf[NT]
						val prob = ruleProbability(r) * pi((i, s, Y)) * pi((s + 1, j, Z));
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

		val parseTrees = determineParseTree(1, n, cfg.startSymbol)
		ParseResult(parseTrees, pi(1, n, cfg.startSymbol))
	}

	def determineParseTree(i: Int, j: Int, symbol: NT): List[ParseTree] = {
		val parseTmps: List[ParseTmp] = bp(i, j, symbol)

		parseTmps.flatMap { parseTmp =>
			val rule = parseTmp.rule
			val s = parseTmp.s

			if (rule.isNonTerminalRule) {
				val leftTrees = determineParseTree(i, s, rule.rightSide(0).asInstanceOf[NT])
				val rightTrees = determineParseTree(s + 1, j, rule.rightSide(1).asInstanceOf[NT])
				for (leftTree <- leftTrees; rightTree <- rightTrees)
					yield ParseTree(rule.leftSide, leftTree, rightTree)
			}
			else {
				List(ParseTree(rule.leftSide, ParseTree(rule.rightSide(0))))
			}
		}
	}
}