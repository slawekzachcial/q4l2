package net.szachcial.q4l2

import org.specs.SpecificationWithJUnit
import java.io.File

class FileIteratorTest extends SpecificationWithJUnit {

	"FileIterator actualPathPattern" should {
		val savedUserName = System.getProperty("user.name")
		val savedUserHome = System.getProperty("user.home")
		val savedUserDir = System.getProperty("user.dir")

		doBefore {
			System.setProperty("user.name", "test_user")
			System.setProperty("user.home", "/home/test_user")
			// The value of "user.dir" property is replaced in the appropriate tests
		}

		doAfter {
			System.setProperty("user.name", savedUserName)
			System.setProperty("user.home", savedUserHome)
			System.setProperty("user.dir", savedUserDir)
		}

		"replace current user's home directory" in {
			new FileIterator("~/somedir".r).actualPathPattern must_== "/home/test_user/somedir"
		}
		"replace other user's home directory" in {
			new FileIterator("~other_user/somedir".r).actualPathPattern must_== "/home/other_user/somedir"
		}
		"replace current directory in UX" in {
			System.setProperty("user.dir", "/unix/path/to")
			new FileIterator("some/dir".r).actualPathPattern must_== "/unix/path/to/some/dir"
		}
		"replace current directory in Windows" in {
			System.setProperty("user.dir", "C:\\windows\\path\\to")
			new FileIterator("some/dir".r).actualPathPattern must_== "C:/windows/path/to/some/dir"
		}
		"handle other paths as-is" in {
			new FileIterator("C:/a/b/c".r).actualPathPattern must_== "C:/a/b/c"
			new FileIterator("/a/b/c".r).actualPathPattern must_== "/a/b/c"
		}
	}

	"FileIterator iterator" should {
		val testDir = new File(getClass.getResource("/path_iterator/marker.txt").getFile).getParent.getPath

		"handle regular expression wildcards" in {
			val pattern = testDir + """/server\d/(20.+txt|acc.+|perf_.+txt)"""
			new FileIterator(pattern.r).toList.map(_.getAbsolutePath) must_==
				List(
					testDir + "/server1/201010.txt",
					testDir + "/server1/201011.txt",
					testDir + "/server1/access.log.001",
					testDir + "/server1/access.log.002",
					testDir + "/server1/access.log.003",
					testDir + "/server1/perf_201010.txt",
					testDir + "/server1/perf_201011.txt",
					testDir + "/server2/201010.txt",
					testDir + "/server2/201011.txt",
					testDir + "/server2/access.log.001",
					testDir + "/server2/access.log.002",
					testDir + "/server2/perf_201010.txt",
					testDir + "/server2/perf_201011.txt"
				)
		}

		"return only existing files" in {
			val pattern = testDir + """/server[1Z]/20101[0Z].txt"""
			new FileIterator(pattern.r).toList.map(_.getAbsolutePath) must_== List(testDir + "/server1/201010.txt")
		}
	}
}