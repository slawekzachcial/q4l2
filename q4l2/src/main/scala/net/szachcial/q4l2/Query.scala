package net.szachcial.q4l2

class Query(tables: List[Table[_]], rowFilter: Expression)


class QuerySyntaxException(msg: String, cause: Throwable = null) extends Exception(msg, cause)


trait Expression extends (List[Row] => Boolean)

case class And(expr: Expression*) extends Expression {
	def apply(candidate: List[Row]): Boolean = expr.forall(_.apply(candidate))
}

case class Or(expr: Expression*) extends Expression {
	def apply(candidate: List[Row]): Boolean = expr.exists(_.apply(candidate))
}

case class Not(expr: Expression) extends Expression {
	def apply(candidate: List[Row]): Boolean = !expr.apply(candidate)
}


trait Operand[T] extends (List[Row] => Option[T])

case class Eq(op1: Operand[_], op2: Operand[_]) extends Expression {
	def apply(candidate: List[Row]): Boolean = (op1(candidate) == op2(candidate))
}

case class Lt[T <% Ordered[T]](op1: Operand[T], op2: Operand[T]) extends Expression {
	def apply(candidate: List[Row]): Boolean = {
		val xOpt = op1(candidate)
		val yOpt = op2(candidate)

		if (xOpt != None && yOpt != None) {
			xOpt.get < yOpt.get
		}
		else {
			false
		}
	}
}

case class Gt[T <% Ordered[T]](op1: Operand[T], op2: Operand[T]) extends Expression {
	def apply(candidate: List[Row]): Boolean = {
		val xOpt = op1(candidate)
		val yOpt = op2(candidate)

		if (xOpt != None && yOpt != None) {
			xOpt.get > yOpt.get
		}
		else {
			false
		}
	}
}

case class Between[T <% Ordered[T]](op: Operand[T], opLeft: Operand[T], opRight: Operand[T]) extends Expression {
	def apply(candidate: List[Row]): Boolean =
		(new Lt(opLeft, op).apply(candidate) && new Lt(op, opRight).apply(candidate))
}

case class Concatenation(operands: Operand[_]*) extends Operand[String] {
	def apply(candidate: List[Row]): Option[String] =
		Some(operands.foldLeft("")((result, oper) => result + oper(candidate).toString))
}

case class Value[T](value: T) extends Operand[T] {
	def apply(candidate: List[Row]): Option[T] = Some(value)
}

//class ColumnValue[T](columnName: String, tableAlias: Option[String]) extends Operand[T] {
//
//	def apply(candidate: List[Row]): Option[T] = {
//		val RowOpt = tableAlias match {
//			case Some(alias) => candidate.find(trc => trc.tableAlias == alias && trc.row.hasColumn(columnName))
//			case None => candidate.find(_.row.hasColumn(columnName))
//		}
//
//		val columnOpt = RowOpt match {
//			case Some(Row) => Row.row.getColumn(columnName)
//			case None => None
//		}
//
//		val value = columnOpt match {
//			case Some(column) => column.value
//			case None => throw new QuerySyntaxException("Column not found: " + columnName)
//		}
//
//		value
//		throw new UnsupportedOperationException("This needs to be re-implemented")
//	}
//
//}

case class ColumnValue[T](column: Column[T]) extends Operand[T] {

	def apply(candidate: List[Row]): Option[T] = {
		val rowOpt = candidate.find(row => row.getColumn(column).isDefined)
		rowOpt match {
			case Some(row) => row.getColumn(column).get.value
			case None => throw new QuerySyntaxException("Column not found: " + column)
		}
	}

}

//class Sum[T <: Number](operands: Operand[T]*) extends Operand[Number] {
//	def apply(candidate: List[Row]) = operands.foldLeft(0)((result, oper) => result + oper(candidate))
//}


//class Concatenation extends (Array[Any] => String) {
//	def apply(value: Array[Any]) = value.foldLeft("")((s,v) => s + v.toString)
//}