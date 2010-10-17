package net.szachcial.q4l2

class Query(tables: List[TableExpression], rowFilter: Expression)


class QuerySyntaxException(msg: String, cause: Throwable = null) extends Exception(msg, cause)

class TableExpression(table: Table[_], alias: Option[String])


class RowCandidate(val tableAlias: Option[String], val row: Row)


trait Expression extends (List[RowCandidate] => Boolean)

class And(expr: Expression*) extends Expression {
	def apply(candidate: List[RowCandidate]): Boolean = expr.forall(_.apply(candidate))
}

class Or(expr: Expression*) extends Expression {
	def apply(candidate: List[RowCandidate]): Boolean = expr.exists(_.apply(candidate))
}

class Not(expr: Expression) extends Expression {
	def apply(candidate: List[RowCandidate]): Boolean = !expr.apply(candidate)
}


trait Operand[T] extends (List[RowCandidate] => Option[T])

class Eq(op1: Operand[_], op2: Operand[_]) extends Expression {
	def apply(candidate: List[RowCandidate]): Boolean = (op1(candidate) == op2(candidate))
}

class Lt[T <: Ordered[T]](op1: Operand[T], op2: Operand[T]) extends Expression {
	def apply(candidate: List[RowCandidate]): Boolean = {
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

class Gt[T <: Ordered[T]](op1: Operand[T], op2: Operand[T]) extends Expression {
	def apply(candidate: List[RowCandidate]): Boolean = {
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

class Between[T <: Ordered[T]](op: Operand[T], opLeft: Operand[T], opRight: Operand[T]) extends Expression {
	def apply(candidate: List[RowCandidate]): Boolean =
		(new Lt(opLeft, op)(candidate) && new Lt(op, opRight)(candidate))
}

class Concatenation(operands: Operand[_]*) extends Operand[String] {
	def apply(candidate: List[RowCandidate]): Option[String] =
		Some(operands.foldLeft("")((result, oper) => result + oper(candidate).toString))
}

class Value[T](value: T) extends Operand[T] {
	def apply(candidate: List[RowCandidate]): Option[T] = Some(value)
}

//class ColumnValue[T](columnName: String, tableAlias: Option[String]) extends Operand[T] {
//
//	def apply(candidate: List[RowCandidate]): Option[T] = {
//		val rowCandidateOpt = tableAlias match {
//			case Some(alias) => candidate.find(trc => trc.tableAlias == alias && trc.row.hasColumn(columnName))
//			case None => candidate.find(_.row.hasColumn(columnName))
//		}
//
//		val columnOpt = rowCandidateOpt match {
//			case Some(rowCandidate) => rowCandidate.row.getColumn(columnName)
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

class ColumnValue[T](column: Column[T]) extends Operand[T] {

	def apply(candidate: List[RowCandidate]): Option[T] = {
		val rows = candidate.map(_.row)
		val rowOpt = rows.find(row => row.getColumn(column).isDefined)
		rowOpt match {
			case Some(row) => row.getColumn(column).get.value
			case None => throw new QuerySyntaxException("Column not found: " + column)
		}
	}

}

//class Sum[T <: Number](operands: Operand[T]*) extends Operand[Number] {
//	def apply(candidate: List[RowCandidate]) = operands.foldLeft(0)((result, oper) => result + oper(candidate))
//}


//class Concatenation extends (Array[Any] => String) {
//	def apply(value: Array[Any]) = value.foldLeft("")((s,v) => s + v.toString)
//}