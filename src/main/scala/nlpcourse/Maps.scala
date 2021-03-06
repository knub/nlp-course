package nlpcourse

import collection.mutable.Map

package object nlpcourse {
	implicit class DoubleKeyMap[A, B, C](m: Map[(A, B), C]) {
		def update(a: A, b: B, c: C) {
			m((a, b)) = c
		}
	}

	implicit class TripleKeyMap[A, B, C, D](m: Map[(A, B, C), D]) {
		def update(a: A, b: B, c: C, d: D) {
			m((a, b, c)) = d
		}
		def apply(a: A, b: B, c: C): D = {
			m((a, b, c))
		}
	}

	type TagList = List[Tag]
	type Word = String
	type Sentence = List[Word]
}
