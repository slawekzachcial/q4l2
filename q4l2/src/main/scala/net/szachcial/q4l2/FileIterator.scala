package net.szachcial.q4l2

import java.io.{FilenameFilter, File}
import util.matching.Regex

class FileIterator(pathRegex: Regex) extends Iterator[File] {
	val actualPathPattern = {
		val CurrentUserPathRegex = "^~(/.*)$".r
		val OtherUserPathRegex = "^~([^/]+)(/.*)$".r
		val AbsolutePathRegex = """^((/|[a-zA-Z]:/).*)$""".r

		pathRegex.toString match {
			case CurrentUserPathRegex(path) => System.getProperty("user.home").replace('\\', '/') + path
			case OtherUserPathRegex(user, path) => System.getProperty("user.home").replace('\\', '/').replace(System.getProperty("user.name"), user) + path
			case AbsolutePathRegex(path, rootDir) => pathRegex.toString
			case _ => System.getProperty("user.dir").replace('\\', '/') + "/" + pathRegex.toString
		}
	}

	private val iterator = {
		class RegexFilenameFilter(regex: Regex) extends FilenameFilter {
			def accept(dir: File, name: String) = regex.findFirstIn(name).getOrElse("").equals(name)
		}

		def findFiles(parentPath: String, remainingPathElements: List[String], result: List[File]): List[File] = {
			val parentFile = new File(parentPath)
			if (!parentFile.exists) {
				Nil
			}
			else if (!parentFile.isDirectory) {
				result ::: List(parentFile)
			}
			else {
				remainingPathElements match {
					case x :: xs =>
						val children = parentFile.listFiles(new RegexFilenameFilter(x.r)).sortWith(_.getName < _.getName)
						children.foldLeft(result)((res, child) =>
							res ++ findFiles(parentPath + child.getName + "/", xs, result)).toList
					case Nil =>
						result ::: List(parentFile)
				}
			}
		}

		val (parentPath, remainingPath) = actualPathPattern.splitAt(actualPathPattern.indexOf('/')+1)
		findFiles(parentPath, remainingPath.split("/").toList, Nil).iterator
	}

	def next = iterator.next
	def hasNext = iterator.hasNext
}