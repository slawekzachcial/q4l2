package net.szachcial.q4l2

import org.junit.Ignore
import org.specs.SpecificationWithJUnit
import java.io.{BufferedReader, StringReader}

@Ignore
class QueryTest


object QueryTestRowParser extends RowParser[QueryTestEntity]("""^(\S+)\t(\d+)""".r, classOf[QueryTestEntity])

class QueryTestEntity extends Row {
	import Nullable._

	object stringColumn extends StringColumn(1, NOT_NULL)
	object intColumn extends IntColumn(2, NOT_NULL)
}

class ColumnValueTest extends SpecificationWithJUnit {

	"ColumnValue" should {
		"throw exception when column not found" in {
			val value = new ColumnValue[Int]("thisColumnDoesNotExist", None)
			val candidate = List(new RowCandidate(None, QueryTestRowParser("abc\t123")))

			value(candidate) must throwA[QuerySyntaxException]
		}

		"fail when value type does not match column type" in {
			val value = new ColumnValue[String]("intColumn", None)
			val candidate = List(new RowCandidate(None, QueryTestRowParser("abc\t123")))

			value(candidate) must throwA[QuerySyntaxException]
		}

		"handle values when called without table alias" in {
			val value = new ColumnValue[String]("stringColumn", None)
			val rowCandidate = List(new RowCandidate(None, QueryTestRowParser("abc\t123")))
			value(rowCandidate).get must_== "abc"

			val value2 = new ColumnValue[Int]("intColumn", None)
			value2(rowCandidate).get must_== 123
		}

	}
}

class QueryTestEntity2 extends Row {
	import Nullable._

	object stringColumn extends StringColumn(1, NOT_NULL)
	object intColumn extends IntColumn(2, NOT_NULL)
}

object QueryTestEntity2 extends QueryTestEntity2 with Table[QueryTestEntity2] {
	val rowRegex = """^(\S+)\t(\d+)""".r
}

class ColumnValueTest2 extends SpecificationWithJUnit {

	def row(rowContent: String): QueryTestEntity2 =
		QueryTestEntity2.iterator(new BufferedReader(new StringReader(rowContent))).next

	"ColumnValue" should {
		"throw exception when column not found" in {
			val value = new ColumnValue2(new StringColumn(-1, Nullable.NOT_NULL) {})
			val rowCandidate = List(new RowCandidate(None, row("abc\t123")))

			value(rowCandidate) must throwA[QuerySyntaxException]
		}
	}

}