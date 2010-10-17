package net.szachcial.q4l2

import org.specs.SpecificationWithJUnit
import Nullable._
import java.text.SimpleDateFormat
import util.matching.Regex

class RowParserTest extends SpecificationWithJUnit {

	val rowParser = new RowParser[MyEntity]("""S1(.+?)( S2(.+))? S3(.+) D1_(.+?)( D2_(.+))?$""".r, classOf[MyEntity])

	"empty line" should {
		"be ignored" in {
			rowParser.parse("") must be(None)
		}
	}

	"non matching line" should {
		"be ignored" in {
			rowParser.parse("# some comment goes here") must be(None)
		}
	}

	"matching line with all fields" should {
		"be properly parsed" in {
			rowParser.parse("S1aaa S2bbb S3ccc D1_2010-08-07 23:20:00,111 D2_2010-08-08 20:23:00,222") match {
				case Some(record) => {
					record.mandatoryString.get must_== "aaa"
					record.optionalString.get must_== "bbb"
					record.uppercaseString.get must_== "CCC"
					record.mandatoryDate.get must_== new SimpleDateFormat("yyyy-MM-dd HH:mm:ss,SSS").parse("2010-08-07 23:20:00,111")
					record.optionalDate.get must_== new SimpleDateFormat("yyyy-MM-dd HH:mm:ss,SSS").parse("2010-08-08 20:23:00,222")
				}
	            case None => fail("Some(record) was expected")
			}
		}
	}

	"matching line with optional fields missing" should {
		"be property parsed when all optional fields are missing" in {
			rowParser.parse("S1aaa S3ccc D1_2010-08-07 23:20:00,111") match {
				case Some(record) => {
					record.mandatoryString.get must_== "aaa"
					record.optionalString.value must be(None)
					record.uppercaseString.get must_== "CCC"
					record.mandatoryDate.get must_== new SimpleDateFormat("yyyy-MM-dd HH:mm:ss,SSS").parse("2010-08-07 23:20:00,111")
					record.optionalDate.value must be(None)
				}
	            case None => fail("Some(record) was expected")
			}
		}

		"be property parsed when some optional fields are missing (1)" in {
			rowParser.parse("S1aaa S3ccc D1_2010-08-07 23:20:00,111 D2_2010-08-08 20:23:00,222") match {
				case Some(record) => {
					record.mandatoryString.get must_== "aaa"
					record.optionalString.value must be(None)
					record.uppercaseString.get must_== "CCC"
					record.mandatoryDate.get must_== new SimpleDateFormat("yyyy-MM-dd HH:mm:ss,SSS").parse("2010-08-07 23:20:00,111")
					record.optionalDate.get must_== new SimpleDateFormat("yyyy-MM-dd HH:mm:ss,SSS").parse("2010-08-08 20:23:00,222")
				}
	            case None => fail("Some(record) was expected")
			}
		}

		"be property parsed when some optional fields are missing (2)" in {
			rowParser.parse("S1aaa S2bbb S3ccc D1_2010-08-07 23:20:00,111") match {
				case Some(record) => {
					record.mandatoryString.get must_== "aaa"
					record.optionalString.get must_== "bbb"
					record.uppercaseString.get must_== "CCC"
					record.mandatoryDate.get must_== new SimpleDateFormat("yyyy-MM-dd HH:mm:ss,SSS").parse("2010-08-07 23:20:00,111")
					record.optionalDate.value must be(None)
				}
				case None => fail("Some(record) was expected")
			}
		}
	}
}

class MyEntity extends Row {
	object mandatoryString extends StringColumn(1, NOT_NULL)
	object optionalString extends StringColumn(3, NULL)
	object uppercaseString extends StringColumn(4, NOT_NULL, s => Some(s.toUpperCase))
	object mandatoryDate extends DateColumn(5, NOT_NULL, "yyyy-MM-dd HH:mm:ss,SSS")
	object optionalDate extends DateColumn(7, NULL, "yyyy-MM-dd HH:mm:ss,SSS")
}
