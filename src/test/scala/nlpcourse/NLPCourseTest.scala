package nlpcourse

import org.scalatest.FunSuite

class NLPCourseTest extends FunSuite {
	def fixture = new ViterbiAlgorithm()
	lazy val D = Tag("D")
	lazy val N = Tag("N")
	lazy val V = Tag("V")

	test("First Viterbi quiz is correctly calculated.") {
		val viterbi = fixture
		viterbi.trainQ(D, Star, Star, 1)
		viterbi.trainQ(N, Star, D, 1)
		viterbi.trainQ(V, D, N, 1)
		viterbi.trainQ(STOP, N, V, 1)
		viterbi.trainE("the", D, 0.8)
		viterbi.trainE("dog", D, 0.2)
		viterbi.trainE("dog", N, 0.8)
		viterbi.trainE("the", N, 0.2)
		viterbi.trainE("barks", V)
		
		viterbi.pi(3, N, V) should equal (0.64)
	}
}
