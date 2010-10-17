package net.szachcial.q4l2

import util.parsing.input.CharSequenceReader
import org.junit.Ignore

@Ignore
class QueryParserTest

object QueryParserTest {
	def main(args: Array[String]) {
		printParsed("seLEct    tbd")
		printParsed("select *")
		printParsed("insert into xyz")
		printParsed("select a,b,c from xyz")
		printParsed("select a,b from x,y")
	}

	def printParsed(query: String) {
		println(query + " ==> " + QueryParser.phrase[Any](QueryParser.query)(new CharSequenceReader(query)))
	}
}