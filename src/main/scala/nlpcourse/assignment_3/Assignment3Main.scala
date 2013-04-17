package nlpcourse

import scala.collection.mutable.{ListBuffer, Map, Set}
import java.io.File
import scalax.io._
import nlpcourse._

object Assignment3 extends App {

	lazy val inputFileName = "gene.train"
	lazy val baseFileName = inputFileName.split("\\.")(0)

	override def main(args: Array[String]): Unit = {
		if (args contains "model1") {
			model1()
		} else if (args contains "calculate1") {
			calculate1()
		}
	}

	def model1() {
		val english = Resource.fromFile("assignment_3/corpus.en")
		val spanish = Resource.fromFile("assignment_3/corpus.es")
		val englishLines = english.lines().toArray
		val spanishLines = spanish.lines().toArray

		val sentences = (0 to englishLines.size - 1).map { i =>
			(spanishLines(i).split(" ").toList, englishLines(i).split(" ").toList)
		}.toList


		val sb = new StringBuilder()
		val em = new EMAlgorithm()
		em.initPhase(sentences)
		em.estimateParams(sentences)

		em.t.foreach { case ((word1, word2), value) =>
			sb.append("%s %s ".format(word1, word2) + value.toString + "\n")
		}
		new File("assignment_3/tValues").delete
		val tFile = Resource.fromFile("assignment_3/tValues")
		tFile.append(sb.toString)
	}

	def calculate1() {
		val tFile = Resource.fromFile("assignment_3/tValues")
		val tLines = tFile.lines()
		val t = Map[(Word, Word), Double]()

		tLines.foreach { line =>
			val lineData = line.split(" ")
			t((lineData(0), lineData(1))) = lineData(2).toDouble
		}

		val dataset = "test"
		val english = Resource.fromFile("assignment_3/%s.en".format(dataset))
		val spanish = Resource.fromFile("assignment_3/%s.es".format(dataset))
		new File("assignment_3/%s.out".format(dataset)).delete
		val out = Resource.fromFile("assignment_3/%s.out".format(dataset))
		val sb = new StringBuilder()
		val englishLines = english.lines().toArray
		val spanishLines = spanish.lines().toArray

		(0 to englishLines.size - 1).foreach { k =>
			val englishWords = englishLines(k).split(" ").zipWithIndex
			spanishLines(k).split(" ").zipWithIndex.foreach { case (spanishWord, i) =>
				val bestAlignment: Int = englishWords.maxBy { case (englishWord, _) =>
					t((spanishWord, englishWord))
				}._2
				sb.append("%d %d %d%n".format(k + 1, bestAlignment + 1, i + 1))
			}
		}
		out.append(sb.toString)
	}
}
