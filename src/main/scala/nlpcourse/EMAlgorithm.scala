package nlpcourse

import nlpcourse._
import scala.collection.mutable.{Map, Set}
import scalaz._; import Scalaz._; import effects._;

class EMAlgorithm {
	var C_e_f = init_C_e_f
	var C_e = init_C_e
	var C_i_l_m = init_C_i_l_m
	var C_j_i_l_m = init_C_j_i_l_m
	var t = Map[(Word, Word), Double]().withDefault { case (word1, word2) =>
		1.0 / possibleForeignWords(word2).size
	}
	var q = Map[(Int, Int, Int, Int), Double]().withDefault { case (j, i, l, m) =>
		1.0 / (l + 1)
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
	def estimateParams(sentences: List[(Sentence, Sentence)], model: Int) {
		val S = 5
		// if (model == 2) {
		// 	println("Initial q values")
		// 	println(q((0, 1, 2, 2)))
		// 	println(q((2, 2, 2, 2)))
		// 	println(q((2, 1, 2, 2)))
		// 	println("Initial q values, done.")
		// }
		(1 to S).foreach { s =>
			println("Starting " + s.toString)
			sentences.zipWithIndex.foreach { case ((words1, words2), k) =>
				words1.zipWithIndex.foreach { case (word1, i) =>
					val words2WithNull = "NULL" :: words2
					words2WithNull.zipWithIndex.foreach { case (word2, j) =>
						val deltaValue = delta(k + 1, words2WithNull, word1, i + 1, words1.length, word2, j, words2.length, model)
						C_e_f((word2, word1)) += deltaValue
						C_e(word2) += deltaValue
						C_i_l_m((i + 1, words2.length, words1.length)) += deltaValue
						C_j_i_l_m((j, i + 1, words2.length, words1.length)) += deltaValue
					}
				}
			}
			C_e_f.foreach { case ((word2, word1), value) =>
				t((word1, word2)) = value / C_e(word2)
			}
			if (model == 2) {
				C_j_i_l_m.foreach { case ((j, i, l, m), value) =>
					q((j, i, l, m)) = value / C_i_l_m((i, l, m))
				}
			}
			// if (model == 2) {
			// 	println("after iteration: " + s)
			// 	println(t)
			// 	println(q)
			// }
			C_e_f = init_C_e_f
			C_e = init_C_e
			C_i_l_m = init_C_i_l_m
			C_j_i_l_m = init_C_j_i_l_m
		}
	}

	def init_C_e = Map[Word, Double]().withDefaultValue(0.0)
	def init_C_e_f = Map[(Word, Word), Double]().withDefaultValue(0.0)
	def init_C_i_l_m = Map[(Int, Int, Int), Double]().withDefaultValue(0.0)
	def init_C_j_i_l_m = Map[(Int, Int, Int, Int), Double]().withDefaultValue(0.0)

	def delta(k: Int, words2WithNull: Sentence, word1: Word, i: Int, m: Int, word2: Word, j: Int, l: Int, model: Int): Double = {
		val value = if (model == 1) {
			t((word1, word2)) / words2WithNull.foldLeft(0.0) { (acc, word) =>
				acc + t((word1, word))
			}
		} else if (model == 2) {
			var index = -1;
			t((word1, word2)) * q((j, i, l, m)) / words2WithNull.foldLeft(0.0) { (acc, word) =>
				index += 1
				acc + t((word1, word)) * q((index, i, l, m))
			}
		} else {
			-1.0
		}
		// if (model == 2) {
		// 	println(k, i, j, value)
		// }
		value
	}
}