import scala.io.Source

class fileReading {
  // reads the txt file as a sequence  
  def txt_one_sequence(file:String):String={
    val sequence = scala.io.Source.fromFile(file).mkString.capitalize
    val final_sequence = sequence.filter(i => i != '\n')
    return final_sequence
  }
  // reads txt file for many sequences 
  def txt_sequences(file:String):Vector[String]={
    val raw = scala.io.Source.fromFile(file).mkString.capitalize
    val RawSequences = raw.split('\n')
    var sequences:Vector[String] = Vector()
    for (i <- 0 to RawSequences.length-1){
      val final_sequence = RawSequences(i).filter(i => i != '\n')
      sequences = sequences :+ final_sequence
    }
    return sequences
  }
  
}
