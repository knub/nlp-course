package nlpcourse

import scala.collection.mutable.{ListBuffer, Map, Set}
import java.io.File
import scalax.io._
import nlpcourse._
import net.liftweb.json
import net.liftweb.json.JsonAST._

object Assignment2 extends App {

	lazy val inputFileName = "gene.train"
	lazy val baseFileName = inputFileName.split("\\.")(0)

	override def main(args: Array[String]): Unit = {
		if (args contains "parsejson") {
			parseJson()
		} else if (args contains "build") {
			val wordsToBeReplaced = determineWordsToBeReplaced
			parseJson(wordsToBeReplaced)
		}
	}

	def parseJson(wordsToBeReplaced: List[String] = List()) {
		val countTrainingFile = Resource.fromFile("assignment_2/parse_train.dat".format(inputFileName))
		// val predictFile = Resource.fromFile("assignment_2/parse_train.dat.rare")
		val sb = new StringBuilder()
		val lines = countTrainingFile.lines()

		lines.toList.take(1).foreach { line =>
			val parsedJson = json.parse(line)
			val parseTree = parseJsonToParseTree(parsedJson)
			println(line)
			println(parseTree)
		}
		// predictFile.append(sb.toString)
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

}
