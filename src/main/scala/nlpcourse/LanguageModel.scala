package nlpcourse

import scala.collection.mutable.{Map, Set}
import nlpcourse._

class LanguageModel {
	private val eValues = Map[(Word, Tag), Double]()
	private val tValues = Map[(Tag, Tag, Tag), Double]()

	private val wordTagOccurrence = Map[(Word, Tag), Int]().withDefaultValue(0)
	private val tagOccurence = Map[Tag, Int]().withDefaultValue(0)

	var wordCount = 0
	val unigramCount = Map[Word, Int]().withDefaultValue(0)
	val bigramCount = Map[(Word, Word), Int]()
	val trigramCount = Map[(Word, Word, Word), Int]()

	var tagCount = 0
	val tagUnigramCount = Map[Tag, Int]().withDefaultValue(0)
	val tagBigramCount = Map[(Tag, Tag), Int]().withDefaultValue(0)
	val tagTrigramCount = Map[(Tag, Tag, Tag), Int]().withDefaultValue(0)

	val tags = Set[Tag]()

	def trainWordTagOccurrence(word: Word, tag: Tag, count: Int) {
		tags += tag
		unigramCount(word) += count
		wordTagOccurrence((word, tag)) += count
		tagOccurence(tag) += count
	}

	def e(w: Word, t: Tag): Double = {
		if (eValues.contains((w, t))) {
			// println("Using given e values.")
			return eValues(w, t)
		}

		val tOccurence = tagOccurence.getOrElse(t, 0)
		// println("Tag %s occcurs %d".format(t, tOccurence))
		// println("Word: %s, Tag: %s, tOccurence: %d, wOccurence: %d".format(w, t, tOccurence, wordTagOccurrence.getOrElse((w, t), 0)))
		if (tOccurence != 0)
			wordTagOccurrence.getOrElse((w, t), 0).toDouble / tOccurence
		else
			0.0
	}

	def trainTagging(s: Sentence, tagList: TagList) {
		tags ++= tagList
		tagCount += tagList.size
		wordCount += s.size

		s.foreach { word => increaseOne(unigramCount, word) }
		s.sliding(2).foreach { word => increaseOne(bigramCount, (word(0), word(1))) }
		s.sliding(3).foreach { word => increaseOne(trigramCount, (word(0), word(1), word(2))) }

		val formedTagList = List(Star, Star) ::: tagList ::: List(STOP)
		formedTagList.foreach { tag => increaseOne(tagUnigramCount, tag) }
		formedTagList.sliding(2).foreach { tags => increaseOne(tagBigramCount, (tags(0), tags(1))) }
		formedTagList.sliding(3).foreach { tags => increaseOne(tagTrigramCount, (tags(0), tags(1), tags(2))) }

		if (s.size == tagList.size) {
			s.zip(tagList).foreach { case (word, tag) =>
				increaseOne(wordTagOccurrence, (word, tag))
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
				if (tValues.contains((tags(0), tags(1), tags(2)))) {
					// println("Using given t value.")
					return tValues((tags(0), tags(1), tags(2)))
				}
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

	def trainTrigramOccurrence(qi_2: Tag, qi_1: Tag, qi: Tag, count: Int) {
		tags += (qi, qi_1, qi_2)
		tagTrigramCount((qi_2, qi_1, qi)) += count
	}
	def trainBigramOccurrence(qi_1: Tag, qi: Tag, count: Int) {
		tags += (qi, qi_1)
		tagBigramCount((qi_1, qi)) += count
	}
	def trainUnigramOccurrence(qi: Tag, count: Int) {
		tags += qi
		tagUnigramCount(qi) += count
	}

	def trainT(qi: Tag, qi_1: Tag, qi_2: Tag, prob: Double) {
		tags += (qi, qi_1, qi_2)
		tValues(qi, qi_1, qi_2) = prob
	}


}