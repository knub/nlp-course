package nlpcourse

import scala.collection.mutable.{ListBuffer, Map, Set}
import java.io.File
import scalax.io._
import nlpcourse._
import net.liftweb.json
import net.liftweb.json.JsonAST._

object Assignment2 {// extends App {

	lazy val inputFileName = "gene.train"
	lazy val baseFileName = inputFileName.split("\\.")(0)

	def mainMethod(args: Array[String]): Unit = {
		if (args contains "parsejson") {
			parseJson()
		} else if (args contains "build") {
			val wordsToBeReplaced = determineWordsToBeReplaced
			parseJson(wordsToBeReplaced)
		} else if (args contains "parse") {
			val cfg = createCFG
			parseSentences(cfg)
		}
	}

	def parseJson(wordsToBeReplaced: List[String] = List()) {
		val countTrainingFile = Resource.fromFile("assignment_2/parse_train_vert.dat".format(inputFileName))
		val rareFileName = "assignment_2/parse_train_vert.dat.rare"
		new File(rareFileName).delete
		val newTrainingFile = Resource.fromFile(rareFileName)
		val sb = new StringBuilder()
		val lines = countTrainingFile.lines()

		lines.toList.foreach { line =>
			val parsedJson = json.parse(line)
			val parseTree = parseJsonToParseTree(parsedJson)
			replaceWords(parseTree, wordsToBeReplaced)
			sb.append("%s%n".format(parseTree.toString))
		}
		newTrainingFile.append(sb.toString)
	}

	def replaceWords(parseTree: ParseTree, wordsToBeReplaced: List[String]) {
		if (parseTree.children.size == 0) {
			val terminal = parseTree.s
			if (wordsToBeReplaced.contains(terminal.name)) {
				parseTree.s = T("_RARE_")
			}
			return
		}
		parseTree.children.foreach { parseTree =>
			replaceWords(parseTree, wordsToBeReplaced)
		}
	}

	def parseJsonToParseTree(json: JValue): ParseTree = {
		json match {
			case JArray(values) if values.length == 3 => {
				ParseTree(NT(values(0).values.toString), parseJsonToParseTree(values(1)), parseJsonToParseTree(values(2)))
			}
			case JArray(values) if values.length == 2 => {
				ParseTree(NT(values(0).values.toString), ParseTree(T(values(1).values.toString)))
			}
			case _ => {
				throw new RuntimeException("Error parsing JSON to ParseTree.")
			}
		}
	}

	def determineWordsToBeReplaced: List[String] = {
		val wordCount = Map[String, Int]().withDefaultValue(0)
		val countTrainingFile = Resource.fromFile("assignment_2/parse_train.count")
		val lines = countTrainingFile.lines()

		lines.toList.filter { line =>
			line.contains("UNARYRULE")
		}.foreach { line =>
			val lineData = line.split(" ")
			val count = lineData(0).toInt
			val word = lineData(3)
			wordCount(word) += count
		}

		wordCount.filter { case (word, wordCount) => wordCount < 5 }.keys.toList
	}

	def createCFG: CFG = {
		val cfg = new CFG()
		val unaryRuleCount = Map[(NT, T), Int]().withDefaultValue(0)
		val binaryRuleCount = Map[(NT, NT, NT), Int]().withDefaultValue(0)
		val nonTerminalCounts = Map[NT, Int]().withDefaultValue(0)

		val countTrainingFile = Resource.fromFile("assignment_2/parse_train_vert.dat.rare.count")
		val lines = countTrainingFile.lines()

		lines.foreach { line =>
			val lineData = line.split(" ")
			if (line contains "UNARYRULE") {
				unaryRuleCount(NT(lineData(2)), T(lineData(3))) += lineData(0).toInt
			}
			else if (line contains "BINARYRULE") {
				binaryRuleCount(NT(lineData(2)), NT(lineData(3)), NT(lineData(4))) += lineData(0).toInt
			}
			else if (line contains "NONTERMINAL") {
				nonTerminalCounts(NT(lineData(2))) += lineData(0).toInt
			}
		}

		val l = ListBuffer[Rule]()

		unaryRuleCount.foreach { case ((nonTerminal, terminal), count) =>
			l += nonTerminal -> (terminal) withProb count.toDouble / nonTerminalCounts(nonTerminal)
		}
		binaryRuleCount.foreach { case ((leftSide, rightSide1, rightSide2), count) =>
			l += leftSide -> (rightSide1, rightSide2) withProb count.toDouble / nonTerminalCounts(leftSide)
		}
		cfg.rules(l.toList)
		cfg.startSymbol = NT("SBARQ")
		cfg
	}

	def parseSentences(cfg: CFG) {
		val developmentFile = Resource.fromFile("assignment_2/parse_dev.dat".format(inputFileName))
		val parseFileName = "assignment_2/parser_test.p3.out"
		new File(parseFileName).delete
		val parseTreeFile = Resource.fromFile(parseFileName)
		val sb = new StringBuilder()
		val parser = new NLPParser(cfg)

		val lines = developmentFile.lines()

		var i = 0
		var l = lines.size
		lines.foreach { line =>
			println("%d/%d".format(i, l))
			i += 1

			val sentence: Sentence = line.split(" ").toList
			val parseTree: ParseTree = parser.parse(sentence).trees(0)
			sb.append("%s%n".format(parseTree.toString))
		}

		parseTreeFile.append(sb.toString)
	}
}
