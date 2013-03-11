package nlpcourse

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class NLPCourseTest extends FunSuite with ShouldMatchers {
	def firstQuiz: ViterbiAlgorithm = {
		val viterbi = new ViterbiAlgorithm()
		viterbi.trainQ(D, Star, Star, 1)
		viterbi.trainQ(N, Star, D, 1)
		viterbi.trainQ(V, D, N, 1)
		viterbi.trainQ(STOP, N, V, 1)
		viterbi.trainE("the", D, 0.8)
		viterbi.trainE("dog", D, 0.2)
		viterbi.trainE("dog", N, 0.8)
		viterbi.trainE("the", N, 0.2)
		viterbi.trainE("barks", V, 1)
		viterbi
	}
	def firstRealQuiz: ViterbiAlgorithm = {
		val viterbi = new ViterbiAlgorithm()
		viterbi.trainQ(D, Star, Star, 1)
		viterbi.trainQ(N, Star, D, 1)
		viterbi.trainQ(V, D, N, 0.5)
		viterbi.trainQ(STOP, D, N, 0.5)
		viterbi.trainQ(D, N, V, 1)
		viterbi.trainQ(N, V, D, 1) 

		viterbi.trainE("the", D, 1)
		viterbi.trainE("dog", N, 1)
		viterbi.trainE("cat", N, 1)
		viterbi.trainE("saw", N, 1 / 3)
		viterbi.trainE("saw", V, 2 / 3)
		viterbi
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

		viterbi.pi(List("the", "cat", "saw", "the", "saw"), 5, D, N)._2 should be (0.64 plusOrMinus 0.0000001)
	}
}
