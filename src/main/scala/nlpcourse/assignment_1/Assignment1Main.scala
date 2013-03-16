package nlpcourse

import scala.collection.mutable.Set
import scalax.io._

object Assignment1 extends App {

	override def main(args: Array[String]): Unit = {
		val inputFileName = "gene.train"

		val trainingFile = Resource.fromFile("assignment_1/%s.counts".format(inputFileName))
		val lm = new LanguageModel
		val lines = trainingFile.lines()

		val wordsToBeReplaced: List[String] = lines.toList.filter { line =>
			line.contains("WORDTAG")
		}.flatMap { line =>
			val lineData = line.split(" ")
			val count = lineData(0).toInt
			val word = lineData(3)
			if (count < 5)
				List(word)
			else
				List()
		}

		val newTrainingFile = Resource.fromFile("assignment_1/%s.new".format(inputFileName))

		lines.foreach { line =>
			val lineData = line.split(" ")
			val s = if (wordsToBeReplaced.contains(lineData(3)))
				"%s %s %s _RARE_%n".format(lineData(0), lineData(1), lineData(2))
			else
				"%s%n".format(line)
			newTrainingFile.append(s)
		}
	}
}
