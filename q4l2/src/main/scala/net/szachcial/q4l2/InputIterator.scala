package net.szachcial.q4l2

import util.matching.Regex
import java.io.{FileReader, BufferedReader}

class InputIterator(
		private val input: BufferedReader,
		val startRecordPattern: Regex = InputIterator.DEFAULT_PATTERN,
		val lineSeparator: String = InputIterator.DEFAULT_LINE_SEPARATOR)
	extends Iterator[String]
{
	private var nextRecord: String = null
	private var moreInputToRead: Boolean = true
	private var afterNextRecordPrefix: String = null


	def next: String = {
		if (!hasNext) {
			throw new NoSuchElementException
		}

		val result = nextRecord
		nextRecord = null
		result
	}

	def hasNext: Boolean = nextRecord != null || findNext

	def findNext: Boolean = {
		if (afterNextRecordPrefix != null) {
			nextRecord = afterNextRecordPrefix
			afterNextRecordPrefix = null
		}

		while (moreInputToRead && afterNextRecordPrefix == null) {
			val line = input.readLine
			moreInputToRead = (line != null)

			if (line != null) {
				startRecordPattern.findPrefixOf(line) match {
					case Some(s) => {
						if (nextRecord == null) nextRecord = line
						else afterNextRecordPrefix = line
					}
					case None => {
						if (nextRecord != null) nextRecord += lineSeparator + line
					}
				}
			}
		}

		nextRecord != null
	}
}

object InputIterator {
	val DEFAULT_PATTERN = "^.".r
	val DEFAULT_LINE_SEPARATOR = "-||-"

	def main(args: Array[String]) {
		val input = new BufferedReader(new FileReader(args(0)))
		val startRegex = if (args.length == 2) new Regex(args(1)) else DEFAULT_PATTERN

		println("File path: " + args(0))
		println("Record start pattern: " + startRegex.pattern)

		val recordIterator: InputIterator = new InputIterator(input, startRegex)
		recordIterator.foreach(println)
	}
}