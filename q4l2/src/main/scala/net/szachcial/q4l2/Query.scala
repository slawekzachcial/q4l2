package net.szachcial.q4l2

class Query(tables: List[Table[_]], rowFilter: Expression[Boolean])


class QuerySyntaxException(msg: String, cause: Throwable = null) extends Exception(msg, cause)


trait Expression[+T] extends (List[Row] => Option[T])

case object Null extends Expression[Nothing] {
	def apply(candidate: List[Row]) = None
}

case class Value[T](value: T) extends Expression[T] {
	def apply(candidate: List[Row]): Option[T] = Some(value)
}

case object True extends Expression[Boolean] {
	def apply(candidate: List[Row]) = Some(true)
}

case object False extends Expression[Boolean] {
	def apply(candidate: List[Row]) = Some(false)
}

case class Not(expression: Expression[Boolean]) extends Expression[Boolean] {
	def apply(candidate: List[Row]) = {
		expression.apply(candidate) match {
			case Some(true) => Some(false)
			case Some(false) => Some(true)
			case None => None
		}
	}
}

case class And(expressions: Expression[Boolean]*) extends Expression[Boolean] {

	def apply(candidate: List[Row]) = evaluate(expressions.toList, candidate, false)

	private def evaluate(expressionList: List[Expression[Boolean]], candidate: List[Row], foundNull: Boolean): Option[Boolean] = {
		expressionList match {
			case x :: xs =>
				x.apply(candidate) match {
					// if found false - the AND is false and no need to continue
					case Some(false) => Some(false)
					// if found true - let's check the rest
					case Some(true) => evaluate(xs, candidate, foundNull)
					// if found None/Null - let's capture this in foundNull and let's continue
					case None => evaluate(xs, candidate, true)
				}
			case Nil =>
				// end of the list analysis - if we got here there was no false in the list
				// if we had Null it is null otherwise it's true
				if (foundNull) None
				else Some(true)
		}
	}
}

case class Or(expressions: Expression[Boolean]*) extends Expression[Boolean] {

	def apply(candidate: List[Row]) = evaluate(expressions.toList, candidate, false)

	private def evaluate(expressionList: List[Expression[Boolean]], candidate: List[Row], foundNull: Boolean): Option[Boolean] = {
		expressionList match {
			case x :: xs =>
				x.apply(candidate) match {
					// if found true - the OR is true and no need to continue
					case Some(true) => Some(true)
					// if found false - let's check the rest
					case Some(false) => evaluate(xs, candidate, foundNull)
					// if found None/Null - let's capture this in foundNull and continue
					case None => evaluate(xs, candidate, true)
				}
			case Nil =>
				// end of the list analysis - if we got here there was no true in the list
				// if we had Null it's null otherwise it's false
				if (foundNull) None
				else Some(false)
		}
	}
}

abstract class Comparison[T](op1: Expression[T], op2: Expression[T]) extends Expression[Boolean] {

	def apply(candidate: List[Row]) = {
		val result1 = op1.apply(candidate)
		val result2 = op2.apply(candidate)
		result1 match {
			// comparing with Null is always Null
			case None => None
			case Some(value1) =>
				result2 match {
					case None => None
					case Some(value2) => Some(evaluate(value1, value2))
				}
		}
	}

	def evaluate(value1: T, value2: T): Boolean
}

case class Eq[T](op1: Expression[T], op2: Expression[T]) extends Comparison[T](op1, op2) {
	def evaluate(value1: T, value2: T) = (value1 == value2)
}

case class Lt[T <% Ordered[T]](op1: Expression[T], op2: Expression[T]) extends Comparison[T](op1, op2) {
	def evaluate(value1: T, value2: T) = (value1 < value2)
}

case class Le[T <% Ordered[T]](op1: Expression[T], op2: Expression[T]) extends Comparison[T](op1, op2) {
	def evaluate(value1: T, value2: T) = (value1 <= value2)
}

case class Gt[T <% Ordered[T]](op1: Expression[T], op2: Expression[T]) extends Comparison[T](op1, op2) {
	def evaluate(value1: T, value2: T) = (value1 > value2)
}

case class Ge[T <% Ordered[T]](op1: Expression[T], op2: Expression[T]) extends Comparison[T](op1, op2) {
	def evaluate(value1: T, value2: T) = (value1 >= value2)
}

case class Between[T <% Ordered[T]](op: Expression[T], opLeft: Expression[T], opRight: Expression[T]) extends Comparison[Boolean](Le(opLeft, op), Le(op, opRight))
{
	def evaluate(value1: Boolean, value2: Boolean) = (value1 && value2)
}

case class ColumnValue[T](column: Column[T]) extends Expression[T] {

	def apply(candidate: List[Row]): Option[T] = {
		val rowOpt = candidate.find(row => row.getColumn(column).isDefined)
		rowOpt match {
			case Some(row) => row.getColumn(column).get.value
			case None => throw new QuerySyntaxException("Column not found: " + column)
		}
	}

}

//case class Concatenation(operands: Expression[_]*) extends Expression[String] {
//	def apply(candidate: List[Row]): Option[String] =
//		Some(operands.foldLeft("")((result, oper) => result + oper(candidate).toString))
//}

//class Sum[T <: Number](operands: Operand[T]*) extends Operand[Number] {
//	def apply(candidate: List[Row]) = operands.foldLeft(0)((result, oper) => result + oper(candidate))
//}


//class Concatenation extends (Array[Any] => String) {
//	def apply(value: Array[Any]) = value.foldLeft("")((s,v) => s + v.toString)
//}