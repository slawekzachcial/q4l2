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
			val artificialColumn = new StringColumn(-1, Nullable.NOT_NULL) {}
			val value = new ColumnValue(artificialColumn)
			val rowCandidate = List(row("abc\t123"))

			value(rowCandidate) must throwA[QuerySyntaxException]
		}

		"retrieve value when column found" in {
			val value = new ColumnValue(QueryTestEntity.intColumn)
			val rowCandidate = List(row("abc\t123"))

			value(rowCandidate) must_== Some(123)
		}
	}

}

class ExpressionTest extends SpecificationWithJUnit {

	"Expression: intColumn between 100 and 200 and stringColumn = 'abc'" should {
		val testExpression = And(
			Between(ColumnValue(QueryTestEntity.intColumn), Value(100), Value(200)),
			Eq(ColumnValue(QueryTestEntity.stringColumn), Value("abc"))
		)

		"return true for ('abc', 150)" in {
			val row = QueryTestEntity.iterator("abc\t150").next
			testExpression(List(row)) must beTrue
		}

		"return false for ('abc', 99)" in {
			val row = QueryTestEntity.iterator("abc\t99").next
			testExpression(List(row)) must beFalse
		}

		"return false for ('def', 120)" in {
			val row = QueryTestEntity.iterator("def\t120").next
			testExpression(List(row)) must beFalse
		}
	}
}