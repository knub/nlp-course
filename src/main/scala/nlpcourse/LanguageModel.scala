package nlpcourse

import scala.collection.mutable.{Map, Set}
import nlpcourse._

class LanguageModel {
	val e = Map[(String, Tag), Double]().withDefaultValue(0.0)
	val q = Map[(Tag, Tag, Tag), Double]().withDefaultValue(0.0)

	val tags = Set[Tag]()

	def trainE(word: String, tag: Tag, prob: Double) {
		e(word, tag) = prob
	}

	def trainQ(qi: Tag, qi_1: Tag, qi_2: Tag, prob: Double) {
		tags += (qi, qi_1, qi_2)
		q(qi, qi_1, qi_2) = prob
	}

	
}