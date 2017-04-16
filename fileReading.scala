import scala.io.Source

class fileReading {
    // reads the txt file as a sequence  
  def txt_sequence(file:String):String={
    val sequence = scala.io.Source.fromFile(file).mkString.capitalize
    val final_sequence = sequence.filter(i => i != '\n')
    return final_sequence
  }
  
}
