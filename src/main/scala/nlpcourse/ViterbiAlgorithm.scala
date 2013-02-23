package nlpcourse

import scala.collection.mutable.{Map, Set}
import nlpcourse._
import scalaz._
import Scalaz._

class ViterbiAlgorithm {
	val e = Map[(String, Tag), Double]().withDefaultValue(0)
	val q = Map[(Tag, Tag, Tag), Double]().withDefaultValue(0)

	val tags = Set[Tag]()
	def trainE(word: String, tag: Tag, prob: Double) {
		e(word, tag) = prob
	}

	def trainQ(qi: Tag, qi_1: Tag, qi_2: Tag, prob: Double) {
		tags += (qi, qi_1, qi_2)
		q(qi, qi_1, qi_2) = prob
	}

	def piBruteForce(sentence: Sentence, k: Int, qi_1: Tag, qi: Tag): Double = {
		possibleTaggings(k).map { tagging =>
			tagging ::: List(qi_1, qi)
		}.map { tagging =>
			r(sentence, tagging)
		}.max
	}

	def pi(sentence: Sentence, k: Int, qi_1: Tag, qi: Tag): Double = {
		-1
	}

	def r(sentence: Sentence, tagging: List[Tag]): Double = {
		(2 to tagging.size - 1).foldLeft(1.0) { (acc, i) =>
			acc * q(tagging(i), tagging(i - 2), tagging(i - 1))
		} *
		(2 to tagging.size - 1).foldLeft(1.0) { (acc, i) =>
			acc * e(sentence(i - 2), tagging(i))
		}

	}

	def possibleTaggings(length: Int): TagList = {
		val tagList = tags.toList.map { tag => List(tag) }
		val crossProductEndo = EndoTo(crossProduct((_: TagList), tagList))
		kthCrossProduct(length - 2, crossProductEndo, tagList).map { tagging =>
			List(Star, Star) ::: tagging
		}
	}

	private def kthCrossProduct(k: Int, endo: Endo[TagList], l: TagList): TagList = {
		// as we want to calculate endo^k, we have to apply endo k - 1 with itself
		(k - 1).times(endo).apply(l)
	}
	private def crossProduct(l1: TagList, l2: TagList): TagList = {
		// val l = List(List(1), List(2), List(3))
		for (x <- l1; y <- l2)
			yield x ::: y
	}

}
