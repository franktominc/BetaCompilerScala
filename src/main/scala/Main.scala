import fastparse.core.Parsed
import fastparse.core.Parsed.Success

/**
  * Created by frank on 19/01/2017.
  */
object Main {

  def main(args: Array[String]): Unit = {
      val c = new CharStream("D:\\Dropbox\\Dropbox\\compilador\\teste.txt")
      val Parsed.Success(value, _) = MyParser.programParser.parse(c.getAll.mkString)
      println(value)
      println()
      val cg = new CodeGenerator(Semantic.Analise(value)._2, value.value._1, value.value._2)
    cg.generate
  }
}
