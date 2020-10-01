package controlstructures

trait InputSource {
  def getStr: String
  def putStr(line: String): Unit
  def putSuccess(line: String): Unit
  def putError(line: String): Unit
}
object InputSource {
  implicit def dsl: InputSource = new InputSource {
    override def getStr: String = scala.io.StdIn.readLine

    override def putStr(line: String): Unit = println(line)

    override def putError(line: String): Unit = putStrInColor(s"ERROR: $line")(scala.Console.RED)

    override def putSuccess(line: String): Unit = putStrInColor(line)(scala.Console.GREEN)

    private def putStrInColor(line: String)(color: String): Unit = putStr(color + line + scala.Console.RESET)
  }
}
