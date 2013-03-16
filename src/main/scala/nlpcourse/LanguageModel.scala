package nlpcourse

import scala.collection.mutable.{Map, Set}
import nlpcourse._

class LanguageModel {
	private val eValues = Map[(String, Tag), Double]()
	private val tValues = Map[(Tag, Tag, Tag), Double]()

	private val stringTagOccurrence = Map[(String, Tag), Int]()
	private val tagOccurence = Map[Tag, Int]()

	var wordCount = 0
	val unigramCount = Map[Word, Int]()
	val bigramCount = Map[(Word, Word), Int]()
	val trigramCount = Map[(Word, Word, Word), Int]()

	var tagCount = 0
	val tagUnigramCount = Map[Tag, Int]()
	val tagBigramCount = Map[(Tag, Tag), Int]()
	val tagTrigramCount = Map[(Tag, Tag, Tag), Int]()


	val tags = Set[Tag]()

	def e(w: String, t: Tag): Double = {
		if (eValues.contains((w, t)))
			return eValues(w, t)

		val tOccurence = tagOccurence.getOrElse(t, 0)
		// println("Word: %s, Tag: %s, tOccurence: %d, wOccurence: %d".format(w, t, tOccurence, stringTagOccurrence.getOrElse((w, t), 0)))
		if (tOccurence != 0)
			stringTagOccurrence.getOrElse((w, t), 0).toDouble / tOccurence
		else
			1.0
	}

	def trainTagging(s: Sentence, tagList: TagList) {
		tags ++= tagList
		tagCount += tagList.size
		wordCount += s.size

		s.foreach { word => increaseOne(unigramCount, word) }
		s.sliding(2).foreach { word => increaseOne(bigramCount, (word(0), word(1))) }
		s.sliding(3).foreach { word => increaseOne(trigramCount, (word(0), word(1), word(2))) }

		val formedTagList = List(Star, Star) ::: tagList ::: List(STOP)
		formedTagList.foreach { word => increaseOne(tagUnigramCount, word) }
		formedTagList.sliding(2).foreach { word => increaseOne(tagBigramCount, (word(0), word(1))) }
		formedTagList.sliding(3).foreach { word => increaseOne(tagTrigramCount, (word(0), word(1), word(2))) }

		if (s.size == tagList.size) {
			s.zip(tagList).foreach { case (word, tag) =>
				increaseOne(stringTagOccurrence, (word, tag))
				increaseOne(tagOccurence, tag)
			}
		}
	}

	private def increaseOne[K](m: Map[K, Int], v: K) {
		if (m.contains(v))
			m(v) += 1
		else
			m(v) = 1
	}
	def t(tags: Tag*): Double = {
		tags.size match {
			case 3 => {
				if (tValues.contains((tags(0), tags(1), tags(2))))
					return tValues((tags(0), tags(1), tags(2)))
			}
		}

		val d = tags.size match {
			case 1 => tagUnigramCount.getOrElse(tags(0), 0).toDouble / tagCount
			case 2 => tagBigramCount.getOrElse((tags(1), tags(0)), 0).toDouble / tagUnigramCount.getOrElse(tags(1), 0)
			case 3 => tagTrigramCount.getOrElse((tags(1), tags(2), tags(0)), 0).toDouble / tagBigramCount.getOrElse((tags(1), tags(2)), 0)
		}
		if (d == Double.PositiveInfinity || d.isNaN)
			0.0
		else
			d
	}

	def trainE(word: String, tag: Tag, prob: Double) {
		eValues(word, tag) = prob
	}

	def trainT(qi: Tag, qi_1: Tag, qi_2: Tag, prob: Double) {
		tags += (qi, qi_1, qi_2)
		tValues(qi, qi_1, qi_2) = prob
	}


}