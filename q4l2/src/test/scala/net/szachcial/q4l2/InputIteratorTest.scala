package net.szachcial.q4l2

import org.specs.SpecificationWithJUnit
import util.matching.Regex
import java.io.{InputStreamReader, StringReader, BufferedReader}

class InputIteratorTest extends SpecificationWithJUnit {

	def asReader(input: String) = new BufferedReader(new StringReader(input))

	def asIterator(input: String) = new InputIterator(asReader(input))

	def asIterator(input: String, pattern: Regex) = new InputIterator(asReader(input), pattern)

	def asIterator(input: String, pattern: Regex, separator: String) = new InputIterator(asReader(input), pattern, separator)

	"iterator without startRecordPattern" should {
		"handle properly empty input" in {
			val iter = asIterator("")
			iter.hasNext must beFalse
			iter.next must throwA[NoSuchElementException]
		}

		"handle properly single-line input" in {
			val iter = asIterator("Single line")
			iter.hasNext must beTrue
			iter.hasNext must beTrue
			iter.next must_== "Single line"
			iter.hasNext must beFalse
			iter.next must throwA[NoSuchElementException]
		}

		"handle properly multi-line input" in {
			val iter = asIterator("a\nb\nc")
			iter.toList must_== List("a", "b", "c")
		}
	}

	"iterator with startRecordPattern" should {
		"handle properly multi-line input with single-line records only" in {
			val iter = asIterator("1a\n2b\n3c", "\\d".r)
			iter.toList must_== List("1a", "2b", "3c")
		}

		"handle properly multi-line input with multi-line records" in {
			val iter = asIterator("1a\naa\n2b\n3c\n4d\ndd\ndd\ndd", "\\d".r)
			iter.toList must_== List("1a-||-aa", "2b", "3c", "4d-||-dd-||-dd-||-dd")
		}

		"skip non-matching leading lines" in {
			val iter = asIterator("aa\n2b\n3c\ncc\n4d", "\\d".r)
			iter.toList must_== List("2b", "3c-||-cc", "4d")
		}
	}

	"iterator with startRecordPattern and lineSeparator" should {
		"use the custom line separator" in {
			val iter = asIterator("1a\naa\n2b", "\\d".r, "!!")
			iter.toList must_== List("1a!!aa", "2b")
		}
	}
}