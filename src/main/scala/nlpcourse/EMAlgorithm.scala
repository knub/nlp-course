package nlpcourse

import nlpcourse._
import scala.collection.mutable.{Map, Set}
import scalaz._; import Scalaz._; import effects._;

class EMAlgorithm {
	var C_e_f = init_C_e_f
	var C_e = init_C_e
	var t = Map[(Word, Word), Double]().withDefault { case (word1, word2) =>
		1.0 / possibleForeignWords(word2).size
	}
	val possibleForeignWords = Map[Word, Set[Word]]().withDefaultValue(Set[Word]())

	def initPhase(sentences: List[(Sentence, Sentence)]) {
		sentences.foreach { case (words1, words2) =>
			("NULL" :: words2).foreach { word =>
				if (!possibleForeignWords.contains(word))
					possibleForeignWords(word) = Set[Word]()
				possibleForeignWords(word) ++= words1
			}
		}
	}

	// second value in the tuple should be language to be translated to
	// (the english ones)
	def estimateParams(sentences: List[(Sentence, Sentence)]) {
		val S = 5
		(1 to 5).foreach { s =>
			sentences.zipWithIndex.foreach { case ((words1, words2), k) =>
				words1.zipWithIndex.foreach { case (word1, i) =>
					val words2WithNull = "NULL" :: words2
					words2WithNull.zipWithIndex.foreach { case (word2, j) =>
						val deltaValue = delta(words2WithNull, word1, word2)
						C_e_f((word2, word1)) += deltaValue
						C_e(word2) += deltaValue
					}
				}
			}
			C_e_f.foreach { case ((word2, word1), value) =>
				t((word1, word2)) = value / C_e(word2)
			}
			C_e_f = init_C_e_f
			C_e = init_C_e
		}
	}

	def init_C_e = Map[Word, Double]().withDefaultValue(0.0)
	def init_C_e_f = Map[(Word, Word), Double]().withDefaultValue(0.0)

	def delta(words2WithNull: Sentence, word1: Word, word2: Word): Double = {
		t((word1, word2)) / words2WithNull.foldLeft(0.0) { (acc, word) =>
			acc + t((word1, word))
		}
	}
}