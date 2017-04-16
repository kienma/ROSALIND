
import scala.io.Source

class RNA (seq: String){
  // reads the text file as a sequence  
  def sequenceFile(file:String):String={
    val sequence = scala.io.Source.fromFile(file).mkString.capitalize
    return sequence
  }
  // transcribes DNA into RNA 
  def transcribe(sequence:String):String={ 
    return sequence.replace('T', 'U')
  }
}
