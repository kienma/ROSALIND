class HAMM(sequence_1: String, sequence_2:String) {
  // computes the hamming distance of two sequences
  def hamming_Distance(seq1:String, seq2:String):BigInt={
    if (seq1.length != seq2.length){
      println("Error: sequences are not the same length")
      return -1 
    }
    var d:BigInt = 0 
    for (i <- 0 to seq1.length-1){
      val S1 = seq1.substring(i, i+1)
      val S2 = seq2.substring(i, i+1)
      if (S1 != S2) {
        d += 1 
      }
    }
    return d 
  }
  
}
