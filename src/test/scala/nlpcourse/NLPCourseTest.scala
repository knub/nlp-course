package nlpcourse

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class NLPCourseTest extends FunSuite with ShouldMatchers {
	def firstInVideoQuiz: ViterbiAlgorithm = {
		val model = new LanguageModel()
		model.trainT(D, Star, Star, 1)
		model.trainT(N, Star, D, 1)
		model.trainT(V, D, N, 1)
		model.trainT(STOP, N, V, 1)
		model.trainE("the", D, 0.8)
		model.trainE("dog", D, 0.2)
		model.trainE("dog", N, 0.8)
		model.trainE("the", N, 0.2)
		model.trainE("barks", V, 1)
		new ViterbiAlgorithm(model)
	}
	def firstQuizQuestionSixLanguageModel: LanguageModel = {
		val model = new LanguageModel()
		model.trainTagging(List("the", "dog", "saw", "the", "cat"), List(D, N, V, D, N))
		model.trainTagging(List("the", "cat", "saw", "the", "saw"), List(D, N, V, D, N))
		model
	}
	lazy val D = Tag("D")
	lazy val N = Tag("N")
	lazy val V = Tag("V")

	test("First viterbi in-video quiz is correctly calculated using brute force algorithm.") {
		val viterbi = firstInVideoQuiz
		viterbi.piBruteForce(List("the", "dog", "barks"), 3, N, V) should be (0.64 plusOrMinus 0.0000001)
	}

	test("First viterbi in-video quiz is correctly calculated using dynamic programming.") {
		val viterbi = firstInVideoQuiz
		viterbi.pi(List("the", "dog", "barks"), 3, N, V)._2 should be (0.64 plusOrMinus 0.0000001)
	}

	test("First viterbi in-video quiz calculates correct tagging.") {
		val viterbi = firstInVideoQuiz

		viterbi.pi(List("the", "dog", "barks"), 3, N, V)._1 should be (List(D, N, V))
	}

	test("Language Model creates correct counts on first quiz, question six.") {
		val model = firstQuizQuestionSixLanguageModel

		model.t(D, Star, Star) should be (1)
		model.t(N, Star, D) should be (1)
		model.t(V, D, N) should be (0.5)
		model.t(STOP, D, N) should be (0.5)
		model.t(D, N, V) should be (1)
		model.t(N, V, D) should be (1)

		model.e("the", D) should be (1)
		model.e("dog", N) should be (0.25)
		model.e("cat", N) should be (0.5)
		model.e("saw", N) should be (0.25)
		model.e("saw", V) should be (1.0)
		// viterbi.pi(List("the", "cat", "saw", "the", "saw"), 5, D, V)._2 should be (0.055 plusOrMinus 0.001)
	}
	test("First quiz, question six is correctly calculated.") {
		val viterbi = new ViterbiAlgorithm(firstQuizQuestionSixLanguageModel)
		viterbi.pi(List("the", "cat", "saw", "the", "saw"), 5, D, V)._2 should be (0.03125)
	}
}
