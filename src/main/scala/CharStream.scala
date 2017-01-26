import java.io.{BufferedReader, FileReader}

/**
  * Created by frank on 19/01/2017.
  */
class CharStream(fileName : String){
  val file = new BufferedReader(
    new FileReader(fileName))
  var eof : Boolean = false

  def next: Option[Char] = {
    file.read() match {
      case -1 => None
      case x => Some(x.toChar)
    }
  }

  def getAll : List[Char] = {
    next match {
      case None => Nil
      case Some(x) => x :: getAll
    }
  }
}
