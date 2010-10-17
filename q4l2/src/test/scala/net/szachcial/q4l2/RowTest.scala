package net.szachcial.q4l2

import org.specs.SpecificationWithJUnit

class RowTest extends SpecificationWithJUnit {

	val testedRow = RowTestEntity.iterator("aaa").next
	val testedRow2 = RowTestEntity2.iterator("xxxyyy").next

	"getColumn" should {
		"return column when column found" in {
			val columnValue = testedRow.getColumn(RowTestEntity.stringColumn) match {
				case Some(column) => column.value
				case None => fail("Some(column) expected")
			}
			columnValue.get must_== "aaa"

			val rte2StringColumn = testedRow2.getColumn(RowTestEntity2.stringColumn).get
			rte2StringColumn.value.get must_== "xxx"

			val rte2AnotherStringColumn = testedRow2.getColumn(RowTestEntity2.anotherStringColumn).get
			rte2AnotherStringColumn.value.get must_== "yyy"
		}

		"return None when column not found" in {
			testedRow.getColumn(RowTestEntity2.anotherStringColumn) must_== None
		}

		"return None when looking for the column with the same name but from different table" in {
			testedRow.getColumn(RowTestEntity2.stringColumn) must_== None
		}
	}
}

class RowTestEntity extends Row {
	import Nullable._
	object stringColumn extends StringColumn(0, NOT_NULL)
}

object RowTestEntity extends RowTestEntity with Table[RowTestEntity] {
	val rowRegex = "aaa".r
}

class RowTestEntity2 extends Row {
	import Nullable._
	object stringColumn extends StringColumn(1, NOT_NULL)
	object anotherStringColumn extends StringColumn(2, NOT_NULL)
}

object RowTestEntity2 extends RowTestEntity2 with Table[RowTestEntity2] {
	val rowRegex = "(xxx)(yyy)".r
}