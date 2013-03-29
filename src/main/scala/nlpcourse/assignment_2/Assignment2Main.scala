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
			parseJson
		}
	}

	def parseJson {
		val countTrainingFile = Resource.fromFile("assignment_2/parse_train.dat".format(inputFileName))
		val lines = countTrainingFile.lines()

		lines.toList.take(1).foreach { line =>
			val parsedJson = json.parse(line)
			val parseTree = parseJsonToParseTree(parsedJson)
			println(line)
			println(parseTree)
		}
	}

	def parseJsonToParseTree(json: JValue): ParseTree = {
		json match {
			case JArray(values) if values.length == 3 => {
				ParseTree(NT(values(0).values.toString), parseJsonToParseTree(values(1)), parseJsonToParseTree(values(2)))
			}
			case JArray(values) if values.length == 2 => {
				ParseTree(T(values(1).values.toString))
			}
			case _ => {
				throw new RuntimeException("Error parsing JSON to ParseTree.")
			}
		}
	}

	def determineWordsToBeReplaced: List[String] = {
		val wordCount = Map[String, Int]().withDefaultValue(0)
		val countTrainingFile = Resource.fromFile("assignment_1/%s.counts".format(inputFileName))
		val lines = countTrainingFile.lines()

		lines.toList.filter { line =>
			line.contains("WORDTAG")
		}.foreach { line =>
			val lineData = line.split(" ")
			val count = lineData(0).toInt
			val word = lineData(3)
			wordCount(word) += count
		}

		wordCount.filter { case (word, wordCount) => wordCount < 5 }.keys.toList
	}

}
