package nlpcourse

import scala.collection.mutable.{Map, Set}
import nlpcourse._

class LanguageModel {
	private val e = Map[(String, Tag), Double]()
	private val q = Map[(Tag, Tag, Tag), Double]()

	val tags = Set[Tag]()

	def e(w: String, t:Tag): Double = {
		e.getOrElse((w, t), 0.0)
	}
	def q(qi: Tag, qi_1: Tag, qi_2: Tag): Double = {
		q.getOrElse((qi, qi_1, qi_2), 0.0)
	}
	def trainE(word: String, tag: Tag, prob: Double) {
		e(word, tag) = prob
	}

	def trainQ(qi: Tag, qi_1: Tag, qi_2: Tag, prob: Double) {
		tags += (qi, qi_1, qi_2)
		q(qi, qi_1, qi_2) = prob
	}

	
}