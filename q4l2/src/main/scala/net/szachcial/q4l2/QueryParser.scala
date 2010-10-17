package net.szachcial.q4l2

import util.parsing.combinator.RegexParsers
import java.io.BufferedReader

object QueryParser extends RegexParsers {

	val NAME = "(?i)[a-zA-Z_][a-zA-Z0-9_]*".r
	val SELECT = "(?i)select".r
	val FROM = "(?i)from".r
	val WHERE = "(?i)where".r
	val GROUP_BY = "(?i)group\\s+by".r
	val HAVING = "(?i)having".r
	val ORDER_BY = "(?i)order\\s+by".r
	val AS = "(?i)as".r

	
	def query: Parser[Query2] =
		selectExpression ~ fromExpression ^^ { case se ~ te => new Query2(se, te) }

	def selectExpression: Parser[Fields] =
		SELECT ~> ("*" | repsep(NAME, ",")) ^^ {
			case "*" => new AllFields
			case fieldNames: List[String] => new Fields(fieldNames)
		}

	def fromExpression: Parser[List[String]] =
		FROM ~> repsep(NAME, ",")
	
//	def query =
//		SELECT ~ ( "*" | repsep(selectExpression, ","))
//		~ FROM ~ repsep(tableExpression, ",")
//		~ opt(WHERE ~ expression)
//		~ opt(GROUP_BY ~ repsep(expression, ",") ~ opt(HAVING ~ expression))
//		~ opt(ORDER_BY ~ order)

//	def selectExpression =
//		expression ~ opt(opt(AS) ~ columnAlias)
//		| tableAlias ~ ".*"
}

class Fields(val fields: Seq[String] = null) {
	override def toString = fields.toString
}

class AllFields extends Fields(null) {
	override def toString = "*"
}

class Query2(fields: Fields, tables: Seq[String]) {
	override def toString = "SELECT " + fields + " FROM " + tables
}