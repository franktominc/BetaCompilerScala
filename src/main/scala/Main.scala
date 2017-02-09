import fastparse.core.Parsed
import fastparse.core.Parsed.Success

/**
  * Created by frank on 19/01/2017.
  */
object Main {

  def main(args: Array[String]): Unit = {
      val c = new CharStream(args(0))
      val Parsed.Success(value, _) = MyParser.programParser.parse(c.getAll.mkString)
      println(value)
      println()
      val semantic = Semantic.Analise(value)
      semantic._1 match{
        case Right(_) => val cg = new CodeGenerator(semantic._2, value.value._1, value.value._2);    cg.generate(args(1))
        case Left(x) => println(x)
      }


  }
}
