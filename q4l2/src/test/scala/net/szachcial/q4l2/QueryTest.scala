package net.szachcial.q4l2

import org.junit.Ignore
import org.specs.SpecificationWithJUnit

@Ignore
class QueryTest


class QueryTestEntity extends Row {
	import Nullable._

	object stringColumn extends StringColumn(1, NOT_NULL)
	object intColumn extends IntColumn(2, NOT_NULL)
}

object QueryTestEntity extends QueryTestEntity with Table[QueryTestEntity] {
	val rowRegex = """^(\S+)\t(\d+)""".r
}

class ColumnValueTest extends SpecificationWithJUnit {

	def row(rowContent: String): QueryTestEntity = QueryTestEntity.iterator(rowContent).next

	"ColumnValue" should {
		"throw exception when column not found" in {
			val value = new ColumnValue(new StringColumn(-1, Nullable.NOT_NULL) {})
			val rowCandidate = List(new RowCandidate(None, row("abc\t123")))

			value(rowCandidate) must throwA[QuerySyntaxException]
		}

		"retrieve value when column found" in {
			val value = new ColumnValue(QueryTestEntity.intColumn)
			val rowCandidate = List(new RowCandidate(None, row("abc\t123")))

			value(rowCandidate) must_== Some(123)
		}
	}

}