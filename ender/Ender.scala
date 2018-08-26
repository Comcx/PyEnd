package ender


import scala.io.StdIn._
import scala.io.Source
import java.io.PrintWriter


class FileContent (val lines: List [String]) {

  def content = lines.foldLeft ("") ((a, l) => a + l + "\n")

}

class Ender (mark: String = "end", tabSize: Int = 4) {

  private def containBlockColon_? (line: String): Boolean = {
    !line.trim.isEmpty && line.trim.last == ':'
  }
  private def containBlockEnd_? (line: String): Boolean = {
    val l = line.trim
    l.length >= mark.length + 1 && l.substring (0, mark.length + 1) == "#" + mark ||
      l == "else:" || l.length >= 5 && l.substring (0, 4) == "elif"
  }

  def readFrom (url: String): FileContent =
    new FileContent (Source.fromFile (url).getLines.toList)

  def save (file: FileContent) = {

    class File
    new File { def to (url: String): Unit = {
        val out = new PrintWriter (url)
        out.print (file.content)
        out.close ()
      }
    }
  } // end save

  def process (url: String): FileContent = {

    var depth: Int = 0
    var flag_block = false
    val lines = readFrom (url).lines

    new FileContent ( for (line <- lines) yield {

      depth = depth + (if (flag_block) 1 else 0)
      flag_block =  if (containBlockColon_? (line)) true else false
      if (containBlockEnd_? (line)) {
        depth = if (depth > 0) depth - 1 else 0
      }
      (" " * tabSize * depth) + line.trim
    })
  } // end process

} // end class Ender










