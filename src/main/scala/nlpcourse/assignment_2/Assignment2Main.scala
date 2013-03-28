package nlpcourse

import scala.collection.mutable.{ListBuffer, Map, Set}
import java.io.File
import scalax.io._
import nlpcourse._
import net.liftweb.json._

object Assignment2 extends App {

	lazy val inputFileName = "gene.train"
	lazy val baseFileName = inputFileName.split("\\.")(0)

	override def main(args: Array[String]): Unit = {
		val testString = """
		["S", ["PP", ["ADP", "In"], ["NP", ["DET", "the"], ["NP", ["ADJ", "late"], ["NOUN", "1700<s"]]]], ["S", ["NP", ["ADJ", "British"], ["NOUN", "convicts"]], ["S", ["VP", ["VERB", "were"], ["VP", ["VERB", "used"], ["S+VP", ["PRT", "to"], ["VP", ["VERB", "populate"], ["WHNP", ["DET", "which"], ["NOUN", "colony"]]]]]], [".", "?"]]]]
		"""
		val parsedJson = parse(testString)
		println(parsedJson)
	}
}
