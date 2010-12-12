package net.szachcial.q4l2

import java.util.Date
import java.text.SimpleDateFormat
import util.matching.Regex
import java.io. {StringReader, BufferedReader}

trait Table[T <: Row] {

	protected def rowRegex: Regex

	protected def rowClass: Class[T] = {
		val className = getClass.getName
		if (className endsWith "$") {
			Class.forName(className.substring(0, className.length - 1)).asInstanceOf[Class[T]]
		}
		else {
			Class.forName(className).asInstanceOf[Class[T]]
		}
	}

	protected def inputIterator(input: BufferedReader) = new InputIterator(input)

	def iterator(input: BufferedReader): Iterator[T] = inputIterator(input).collect(new RowParser[T](rowRegex, rowClass))

	def iterator(input: String): Iterator[T] = iterator(new BufferedReader(new StringReader(input)))
}


abstract class Row {

	val columns: Seq[(String, Column[_])] = {
		getClass.getMethods
				.filter(method => method.getParameterTypes.isEmpty && classOf[Column[_]].isAssignableFrom(method.getReturnType))
				.map(method => (method.getName, method.invoke(this).asInstanceOf[Column[_]]))
				.toSeq
	}

	// This method is a result of trade off between object state consistency and the client API cleanness.
	// The columns should normally be populated in the constructor - this way it is impossible to create
	// a row without consistent content of the columns. But in order to do so, the Row constructor would
	// require a regexMatch parameter. This in turn would leak into each class extending the Row class -
	// each of the extending classes would need to declare a similar constructor, but this is an implementation
	// detail not important from the client perspective.
	// Separating object creation from the column populating allows the extending classes to not define any
	// constructor parameters.
	def populateColumns(regexMatch: Regex.Match): Row = {
		val canPopulateAllColumns =
			columns.forall {
				case (columnName, column) => column.extract(regexMatch)
			}

		if (!canPopulateAllColumns) {
			throw new IllegalArgumentException("Unable to create Row instance based on the following data: " + regexMatch)
		}

		this
	}

	// FIXME (slawek) - Should this method be here or in Table?

	def getColumn[T](column: Column[T]): Option[Column[T]] = {
		val columnsOnly = columns.map {
			case (name, columnInRow) => columnInRow
		}

		// In order for the next line to work I have to declare row columns as "object columnName extends ColumnType(...)".
		// Doing "val columnName = ColumnType(...)" I wouldn't be able to differentiate, based on the incoming "column"
		// parameter the columns of the same type.
		val columnOpt = columnsOnly.find(_.getClass == column.getClass)
		columnOpt match {
			// As I know from the line above that the class is the same, I can safely cast it
			case Some(col) => Some(col.asInstanceOf[Column[T]])
			case None => None
		}
	}
}


class RowParser[T <: Row](val rowRegex: Regex, val rowClass: Class[T]) extends PartialFunction[String, T] {

	private var lastSeenRowString: String = null
	private var lastSeenRow: Option[T] = null

	def isDefinedAt(recordString: String): Boolean = {
		if (recordString != lastSeenRowString) {
			lastSeenRowString = recordString
			lastSeenRow = parse(recordString)
		}

		lastSeenRow != None
	}

	def apply(rowString: String): T = {
		if (isDefinedAt(rowString) && lastSeenRow != None) {
			lastSeenRow.get
		}
		else {
			throw new IllegalArgumentException("Unable to parse: " + rowString)
		}
	}

	def parse(rowString: String) : Option[T] = {
		rowRegex.findFirstMatchIn(rowString) match {
			case Some(regexMatch) => {
				try {
					val row = rowClass.newInstance.asInstanceOf[T]
					row.populateColumns(regexMatch)
					Some(row)
				}
				catch {
					case e =>
						//FIXME (slawek) - add logger
						e.printStackTrace
						None
				}
			}
			case None => None
		}
	}

}

object Nullable extends Enumeration {
	type Nullable = Value
	val NULL, NOT_NULL = Value
}

import Nullable._

abstract class Column[T](
		val rowRegexGroupIdx: Int,
		val nullable: Nullable,
		val transformer: (String) => Option[T])
{
	protected var extractedValue: Option[T] = None

	protected[q4l2] def extract(regexMatch: Regex.Match): Boolean = {
		val value = regexMatch.group(rowRegexGroupIdx)
		if (value == null && nullable == NOT_NULL) {
			false
		}
		else {
			extractedValue = transformer(value)
			if (nullable == NOT_NULL && extractedValue == None) {
				false
			}
			else {
				true
			}
		}
	}

	def isNull = extractedValue == None

	def value: Option[T] = extractedValue

	override def toString = extractedValue.toString

	implicit def columnToOption[T](column: Column[T]): Option[T] = column.value
}


class Constant(val valueRefName: String) extends Column[String](-1, NOT_NULL, s => Some(s)) {

	override protected[q4l2] def extract(regexMatch: Regex.Match): Boolean = {
		//get the value from the context based on valueRefName
		extractedValue = Some("TBD(" + valueRefName + ")")
		true
	}
}


abstract class StringColumn(
		override val rowRegexGroupIdx: Int,
		override val nullable: Nullable,
		override val transformer: (String) => Option[String] = s => if (s != null) Some(s) else None)
	extends Column[String](rowRegexGroupIdx, nullable, transformer)

abstract class DateColumn(
		override val rowRegexGroupIdx: Int,
		override val nullable: Nullable,
		val datePattern: String)
	extends Column[Date](rowRegexGroupIdx, nullable, s => if (s != null) Some(new SimpleDateFormat(datePattern).parse(s)) else None)

abstract class IntColumn(
		override val rowRegexGroupIdx: Int,
		override val nullable: Nullable,
		override val transformer: (String) => Option[Int] = {s => if (s != null) Some(s.toInt) else None})
	extends Column[Int](rowRegexGroupIdx, nullable, transformer)