import scala.io.Source

class fileReading {
  
  // reads the txt file as a sequence  
  def txt_sequence1(file:String):String={
    val sequence = scala.io.Source.fromFile(file).mkString.capitalize
    val final_sequence = sequence.filter(i => i != '\n')
    return final_sequence
  }
  
  // reads txt file for many lines 
  def txt_sequences(file:String):Vector[String]={
    val raw = scala.io.Source.fromFile(file).mkString.capitalize
    val RawLines = raw.split('\n')
    var lines:Vector[String] = Vector()
    for (i <- 0 to RawLines.length-1){
      val fin_line = RawLines(i).filter(i => i != '\n') // processed line, without the indent 
      lines = lines :+ fin_line
    }
    return lines
  }
  
}
