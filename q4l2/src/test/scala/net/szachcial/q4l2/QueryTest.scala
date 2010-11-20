package net.szachcial.q4l2

import org.junit.Ignore
import org.specs.SpecificationWithJUnit

@Ignore
class QueryTest

class NotTest extends SpecificationWithJUnit {
	"'Not'" should {
		"return true for false" in {
			Not(False).apply(Nil) must_== Some(true)
		}
		"return false for true" in {
			Not(True).apply(Nil) must_== Some(false)
		}
		"return None for Null" in {
			Not(Null).apply(Nil) must be(None)
		}
	}
}

class AndTest extends SpecificationWithJUnit {
	"'And'" should {
		"return true if trues only" in {
			And(True, True, True).apply(Nil) must_== Some(true)
		}
		"return false if any false" in {
			And(True, False, True).apply(Nil) must_== Some(false)
			And(True, Null, False).apply(Nil) must_== Some(false)
		}
		"return None if nulls and trues only" in {
			And(True, Null, True).apply(Nil) must_== None
			And(Null, Null, Null).apply(Nil) must_== None
		}
	}
}

class OrTest extends SpecificationWithJUnit {
	"'Or'" should {
		"return false if falses only" in {
			Or(False, False, False).apply(Nil) must_== Some(false)
		}
		"return true if any true" in {
			Or(False, True, False).apply(Nil) must_== Some(true)
			Or(False, Null, True).apply(Nil) must_== Some(true)
		}
		"return None if nulls and falses only" in {
			Or(False, Null, False).apply(Nil) must_== None
			Or(Null, Null, Null).apply(Nil) must_== None
		}
	}
}

class EqTest extends SpecificationWithJUnit {
	"'Eq'" should {
		"return true if its operands are equal" in {
			Eq(Value("xyz"), Value("xyz")).apply(Nil) must_== Some(true)
		}
		"return false if its operands are different" in {
			Eq(Value("abc"), Value("xyz")).apply(Nil) must_== Some(false)
		}
		"return None if any operand is Null" in {
			Eq(Null, Value("abc")).apply(Nil) must_== None
			Eq(Value("abc"), Null).apply(Nil) must_== None
			Eq(Null, Null).apply(Nil) must_== None
		}
	}
}

class LtTest extends SpecificationWithJUnit {
	"'Lt'" should {
		"return true for 1 < 2" in {
			Lt(Value(1), Value(2)).apply(Nil) must_== Some(true)
		}
		"return false for 2 < 1" in {
			Lt(Value(2), Value(1)).apply(Nil) must_== Some(false)
		}
		"return None if any operand is Null" in {
			Lt(Value(1), Null).apply(Nil) must_== None
			Lt(Null, Value(1)).apply(Nil) must_== None
		}
	}
}


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
			testExpression(List(row)) must_== Some(true)
		}

		"return false for ('abc', 99)" in {
			val row = QueryTestEntity.iterator("abc\t99").next
			testExpression(List(row)) must_== Some(false)
		}

		"return false for ('def', 120)" in {
			val row = QueryTestEntity.iterator("def\t120").next
			testExpression(List(row)) must_== Some(false)
		}
	}
}
