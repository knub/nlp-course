package nlpcourse

import scala.collection.mutable.{Map, Set}
import nlpcourse._
import scalaz._
import Scalaz._

class ViterbiAlgorithm(languageModel: LanguageModel) {
	/**
	 * Implementing Viterbi as of http://www.cs.columbia.edu/~mcollins/hmms-spring2013.pdf
	 */
	 val m = languageModel

	val piValues = Map[(Int, Tag, Tag), Double]().withDefaultValue(1.0)
	val bpValues = Map[(Int, Tag, Tag), Tag]()

	def piBruteForce(sentence: Sentence, k: Int, qi_1: Tag, qi: Tag): Double = {
		possibleTaggingsOfLength(k).map { tagging =>
			tagging ::: List(qi_1, qi)
		}.map { tagging =>
			r(sentence, tagging)
		}.max
	}

	def p(s: Sentence): (TagList, Double) = {
		val n = s.size
		val possibleLastTags = for (u <- K(n - 1); v <- K(n))
			yield (u, v)

		possibleLastTags.map { case (u, v) =>
			(pi(s, n, u, v), u, v)
		}.maxBy { case (pi, u, v) =>
			 pi._2 * m.t(STOP, u, v)
		}._1
	}

	def pi(sentence: Sentence, k: Int, qi_1: Tag, qi: Tag): (TagList, Double) = {
		sentence.zipWithIndex.foreach { case (word, k) =>
			for(u <- K(k - 1); v <- K(k)) {
				val maxTaggingSequence = {
					possibleTagsForPosition(k - 2).map { w =>
						(w, piValues(k - 1, w, u) * m.t(v, w, u) * m.e(sentence(k), v))
					}.maxBy { tagPair =>
						tagPair._2
					}
				}
				bpValues(k, u, v) = maxTaggingSequence._1
				piValues(k, u, v) = maxTaggingSequence._2
			}
		}
		val n = sentence.size
		val lastIndex = n - 1
		val possibleTags = for(u <- K(n - 1); v <- K(n))
			yield (u, v)

		val maxTaggingSequence = possibleTags.map { case (u, v) =>
			(List(u, v), piValues(n - 1, u, v) * m.t(STOP, u, v))
		}.maxBy { taggingSequence => taggingSequence._2 }
		val taggingSequence = new Array[Tag](n)
		taggingSequence(lastIndex) = maxTaggingSequence._1.last
		taggingSequence(lastIndex - 1) = maxTaggingSequence._1.head
		(0 to lastIndex - 2).reverse.foreach { k =>
			taggingSequence(k) = bpValues(k + 2, taggingSequence(k + 1), taggingSequence(k + 2))
		}
		(taggingSequence.toList, maxTaggingSequence._2)
	}

	def r(sentence: Sentence, tagging: List[Tag]): Double = {
		(2 to tagging.size - 1).foldLeft(1.0) { (acc, i) =>
			acc * m.t(tagging(i), tagging(i - 2), tagging(i - 1))
		} *
		(2 to tagging.size - 1).foldLeft(1.0) { (acc, i) =>
			acc * m.e(sentence(i - 2), tagging(i))
		}

	}

	def K = possibleTagsForPosition _
	def possibleTagsForPosition(k: Int): List[Tag] = {
		if (k < 0)
			List(Star)
		else
			createList
	}

	def createList: List[Tag] = {
		(m.tags -- Set(STOP, Star)).toList
	}

	def possibleTaggingsOfLength(length: Int): List[TagList] = {
		val tagList = createList.map { tag => List(tag) }
		val crossProductEndo = EndoTo(crossProduct((_: List[TagList]), tagList))
		kthCrossProduct(length - 2, crossProductEndo, tagList).map { tagging =>
			List(Star, Star) ::: tagging
		}
	}

	private def kthCrossProduct(k: Int, endo: Endo[List[TagList]], l: List[TagList]): List[TagList] = {
		// as we want to calculate endo^k, we have to apply endo k - 1 with itself
		(k - 1).times(endo).apply(l)
	}
	private def crossProduct(l1: List[TagList], l2: List[TagList]): List[TagList] = {
		// val l = List(List(1), List(2), List(3))
		for (x <- l1; y <- l2)
			yield x ::: y
	}

}
