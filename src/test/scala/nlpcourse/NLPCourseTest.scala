package nlpcourse

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class NLPCourseTest extends FunSuite with ShouldMatchers {
	def firstQuiz: ViterbiAlgorithm = {
		val model = new LanguageModel()
		model.trainQ(D, Star, Star, 1)
		model.trainQ(N, Star, D, 1)
		model.trainQ(V, D, N, 1)
		model.trainQ(STOP, N, V, 1)
		model.trainE("the", D, 0.8)
		model.trainE("dog", D, 0.2)
		model.trainE("dog", N, 0.8)
		model.trainE("the", N, 0.2)
		model.trainE("barks", V, 1)
		new ViterbiAlgorithm(model)
	}
	def firstRealQuiz: ViterbiAlgorithm = {
		val model = new LanguageModel()
		model.trainQ(D, Star, Star, 1)
		model.trainQ(N, Star, D, 1)
		model.trainQ(V, D, N, 0.5)
		model.trainQ(STOP, D, N, 0.5)
		model.trainQ(D, N, V, 1)
		model.trainQ(N, V, D, 1) 

		model.trainE("the", D, 1)
		model.trainE("dog", N, 1)
		model.trainE("cat", N, 1)
		model.trainE("saw", N, 1 / 3.0)
		model.trainE("saw", V, 2 / 3.0)
		new ViterbiAlgorithm(model)
	}
	lazy val D = Tag("D")
	lazy val N = Tag("N")
	lazy val V = Tag("V")

	test("First Viterbi quiz is correctly calculated using brute force algorithm.") {
		val viterbi = firstQuiz
		viterbi.piBruteForce(List("the", "dog", "barks"), 3, N, V) should be (0.64 plusOrMinus 0.0000001)
	}

	test("First Viterbi quiz is correctly calculated using dynamic programming.") {
		val viterbi = firstQuiz
		viterbi.pi(List("the", "dog", "barks"), 3, N, V)._2 should be (0.64 plusOrMinus 0.0000001)
	}

	test("First Viterbi quiz calculates correct tagging.") {
		val viterbi = firstQuiz
		
		viterbi.pi(List("the", "dog", "barks"), 3, N, V)._1 should be (List(D, N, V))
	}

	test("First quiz, sixth question") {
		val viterbi = firstRealQuiz

		viterbi.pi(List("the", "cat", "saw", "the", "saw"), 5, D, V)._2 should be (0.055 plusOrMinus 0.001)
	}
}
