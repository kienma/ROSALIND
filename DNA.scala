import scala.io.Source

class DNA (file:String){
  // reads the text file as a sequence  
  def sequenceFile(file:String):String={
    val sequence = scala.io.Source.fromFile(file).mkString.capitalize
    return sequence
  }
  // given a string and letter, counts the number of occurrences of the letter in the string 
  def letterCount(letter:String, sequence:String):BigInt={
    var count:BigInt = 0 
    for (i <- 0 until sequence.length){
      val read = sequence.substring(i,i+1)
      if (read == letter){
        count += 1 
      }
    }
    return count 
  }
  
  def ACGT_count(seq:String):Vector[BigInt]={
    val A = letterCount("A", seq) // counts number of adenine bases 
    val C = letterCount("C", seq) // counts number of cytosine bases 
    val G = letterCount("G", seq) // counts number of guanine bases 
    val T = letterCount("T", seq) // counts number of thymine bases 
    return Vector(A, C, G, T)
  }

}
