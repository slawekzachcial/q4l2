package net.szachcial.q4l2

import java.util.Date
import util.matching.Regex
import java.text.SimpleDateFormat
import collection.mutable.ListBuffer

class ScratchPad

class Qry[T](projection: T) extends Iterator[T] {
	def next = projection
	def hasNext = true
}

class SP_ResultColumn[T](expression: Expression[T]) {
	private var columnValue: Option[T] = throw new IllegalStateException("Value not initialized yet")

	private[q4l2] def setExpressionValue(candidate: List[Row]): Unit = {
		columnValue = expression.apply(candidate)
	}

	def value = columnValue
}

// !!! I want to have client API see only parameterless constructor and companion object to be able to create
// SP_Result with specified named columns

trait SP_Result {
	val columns: Seq[(String, SP_ResultColumn[_])] = {
			getClass.getMethods
					.filter(method => method.getParameterTypes.isEmpty && classOf[SP_ResultColumn[_]].isAssignableFrom(method.getReturnType))
					.map(method => (method.getName, method.invoke(this).asInstanceOf[SP_ResultColumn[_]]))
					.toSeq
	}

	def as[T](expr: Expression[T]) = new SP_ResultColumn(expr)

}

private class TypeUnsafeSP_Result(unsafeColumns: List[(String, Expression[_])]) extends SP_Result {
	override val columns =
		unsafeColumns.map{
			case (name, expression) => (name, as(expression))
		}.toSeq
}

object SP_Result extends SP_Result {

	def apply(columns: (String, Expression[_])*): SP_Result = new TypeUnsafeSP_Result(columns.toList)

}

object Qry {
	def sample {
		val q = new Qry(new SP_Result {
		    val c1 = as(And(Value(true), Null))
			val c2 = as(Value(123))
		})

		val rs = q.next
		rs.c1
		rs.c2
	}
}

/*
object Log4jLevel extends Enumeration {
	val DEBUG, INFO, WARN, ERROR, FATAL = Value
}

class Log3(
		val recordStartRE: Option[Regex] = None,
		val skipLines: Int = 0)

// Idea is to define a regex for the entire log record and then have the extractors reference only the regex groups.
class Log2(val tableRowRegex: Regex)


class Extractor(val fieldNumber: Int, val regex: Regex, val regexGroup: Int)

class DateExtractor(val fieldNumber: Int, val regex: Regex, val regexGroup: Int, val conversionPattern: String)
	extends Extractor(fieldNumber, regex, regexGroup)

class StringExtractor(val fieldNumber: Int, val regex: Regex, val regexGroup: Int)
	extends Extractor(fieldNumber, regex, regexGroup)

class VignetteLog extends Log3(Some("""^\d\d\d\d-\d\d-\d\d \d\d:\d\d:\d\d,\d\d\d""".r)) {
	object timestamp extends DateExtractor(1, """^\d\d\d\d-\d\d-\d\d \d\d:\d\d:\d\d,\d\d\d""".r, 0, "yyyy-MM-dd HH:mm:ss,SSS")
	object thread extends StringExtractor(2, """\[(.+)\] (DEBUG|INFO|WARN|ERROR|FATAL""".r, 1)
	val parentThread: String
	val level: String /*Log4jLevel*/
	val component: String
	val site: String
	val message: String
	val details: String
}

class WebLogicAccessLog extends Log3(skipLines = 4) {
	val timestamp: Date
	val method: String
	val uri: String
	val status: Int
}

//------------------------

object ThinkingAloud {
	val vapLogRE = """^([^\[]+) \[(.+)\] (DEBUG|INFO|WARN|ERROR|FATAL) ([^ ]+)  - \[(.*)\] - (.+)(\|\|(.*))?$""".r
}
*/

/*
class Extracted[T](val regexGroupIdx: Int, val transformer: (String) => T) {
	private var valueContent : T;

	def set(extractedString: String) {
		valueContent = transformer(extractedString)
	}
	
	def value = valueContent
}

class ExtractedString(val regexGroupIdx: Int) extends Extracted[String](regexGroupIdx, s => s)
class ExtractedDate(val regexGroupIdx: Int, val pattern: String) extends Extracted[Date](regexGroupIdx, new SimpleDateFormat(pattern).parse)

class NullableExtractedString(val regexGroupIdx: Int) extends Extracted[Option[String]](regexGroupIdx, s => s)

class Log[T](val regex: Regex) {
	def iterator(input: Iterator[String]): Iterator[T] = input.map(parseRecord)

	private def parseRecord(recordContent: String) : T = {
		
		val record = create

		record
	}

	abstract protected def create: T
}

class VapLog {
	object timestamp extends ExtractedDate(1, "pattern goes here")
	object thread extends ExtractedString(2)
	object level extends ExtractedString(3)
	object component extends ExtractedString(4)
	object site extends NullableExtractedString(5)
	object message extends ExtractedString(6)
	object details extends NullableExtractedString(7)
}

object VapLog extends Table[VapLog]("""^([^\[]+) \[(.+)\] (DEBUG|INFO|WARN|ERROR|FATAL) ([^ ]+)  - \[(.*)\] - (.+)(\|\|(.*))?$""".r) {
	//FIXME - don't like this !!! Prefer not to have to define this
	override protected def create = new VapLog
}

*/