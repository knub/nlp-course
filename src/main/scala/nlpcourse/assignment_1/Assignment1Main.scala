package nlpcourse

import scala.collection.mutable.{Set, Map}
import scalax.io._

object Assignment1 extends App {

	def inputFileName = "gene.train"

	lazy val wordCount = Map[String, Int]().withDefaultValue(0)

	override def main(args: Array[String]): Unit = {
		if (args contains "build") {
			print("Determing words to be replaced")
			val wordsToBeReplaced = determineWordsToBeReplaced
			println(": Done.")
			print("Build new training file")
			buildNewTrainingFile(wordsToBeReplaced)
			println(": Done.")
		}
		else if (args contains "predict") {
			print("Predicting tags")
			predictTags
			println(": Done.")
		}
	}

	def determineWordsToBeReplaced: List[String] = {
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

	def buildNewTrainingFile(wordsToBeReplaced: List[String]) {
		val trainingFile = Resource.fromFile("assignment_1/%s".format(inputFileName))
		val newTrainingFile = Resource.fromFile("assignment_1/%s.new".format(inputFileName))
		val lines = trainingFile.lines()

		val sb = new StringBuilder
		lines.foreach { line =>
			val lineData = line.split(" ")
			val s = if (wordsToBeReplaced.contains(lineData(0)))
				"_RARE_ %s%n".format(lineData.drop(1).mkString(" "))
			else
				"%s%n".format(line)
			sb.append(s)
		}
		newTrainingFile.append(sb.toString)
	}

	def predictTags {
		val countFile = Resource.fromFile("assignment_1/%s.new.counts".format(inputFileName))
		val predictFile = Resource.fromFile("assignment_1/%s_dev.p1.out")
		val countLines = countFile.lines()

		val sb = new StringBuilder
		countLines.foreach { line =>
			val lineData = line.split(" ")
			val count = lineData(0).toInt
			val word = lineData(3)
			wordCount(word) += count
		}
		predictFile.append(sb.toString)


	}
}
