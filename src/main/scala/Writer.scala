import java.io.{BufferedWriter, FileWriter}

/**
  * Created by ftominc on 27/01/17.
  */
class Writer(filename: String) {
  private val file = new BufferedWriter(new FileWriter(filename))

  def write(s: String): Unit = file.write(s)
  def close = file.close()
}
