package nlpcourse

import scala.collection.mutable.{Set, Map}
import java.io.File
import scalax.io._

object Assignment1 extends App {

	lazy val inputFileName = "gene.train"
	lazy val baseFileName = inputFileName.split("\\.")(0)

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

	def buildNewTrainingFile(wordsToBeReplaced: List[String]) {
		val trainingFile = Resource.fromFile("assignment_1/%s".format(inputFileName))
		new File("assignment_1/%s.new".format(inputFileName)).delete
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

	lazy val evaluationFile = "test"
	def predictTags {
		val countFile = Resource.fromFile("assignment_1/%s.new.counts".format(inputFileName))
		val devFile = Resource.fromFile("assignment_1/gene.%s".format(evaluationFile))
		new File("assignment_1/gene_%s.p1.out".format(evaluationFile)).delete
		val predictFile = Resource.fromFile("assignment_1/gene_%s.p1.out".format(evaluationFile))
		val countLines = countFile.lines()
		val devLines = devFile.lines()

		val model = new LanguageModel
		countLines.toList.filter { line =>
			line.contains("WORDTAG")
		}.foreach { line =>
			val lineData = line.split(" ")
			val count = lineData(0).toInt
			val tag = Tag(lineData(2))
			val word = lineData(3)
			// println("Training word: %s, tag: %s, count: %d".format(word, tag, count))
			model.trainWordTagOccurrence(word, tag, count)
		}

		val sb = new StringBuilder
		val tags = model.tags
		devLines.foreach { line =>
			if (line.size == 0)
				sb.append("%s%n".format(line))
			else {
				val word = if (model.unigramCount(line) < 5) "_RARE_" else line
				// println("Word: %s, UnigramCount: %d, UnigramCountRare: %d".format(word, model.unigramCount(line), model.unigramCount("_RARE_")))
				// println("Trying to find tag for %s out of %s".format(word, tags.toString))
				val (predictedTag: Tag, _) = tags.map { tag =>
					// println("Tag %s has probability %f".format(tag, model.e(word, tag)))
					(tag, model.e(word, tag))
				}.maxBy(_._2)
				sb.append("%s %s%n".format(line, predictedTag.tag))
			}
		}
		predictFile.append(sb.toString)
	}
}
