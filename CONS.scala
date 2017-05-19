

class CONS(FASTA:Vector[String]) {
  // gets rid of ID sequences in FASTA format
  def sequences_only(FASTA:Vector[String]):Vector[String]={
    val seq = FASTA.filter(i => i.contains('>')==false)
    return seq
  }
  val S = sequences_only(FASTA) // sequences only from FASTA 
  // ANC is short for 'aligned character count'
  def ANC(character:Char,I:Int):Int={
    var count:Int = 0
    val L = S.length-1
    for (i <- 0 to L){
      val C = S(i)(I)
      if (C == character){
        count += 1 
      }
    }
    return count
  }
  val DNA_bases = Vector('A','C','G','T')
  // character counts for an aligned series of sequences in order A,C,G,T
  def charCount(characters:Vector[Char]):Vector[Vector[Int]]={
    var count:Vector[Vector[Int]] = Vector()
    val L = S(0).length-1
    for (i <- 0 to L){
      var nuc:Vector[Int] = Vector()
      for (j <- characters){
        val anc = ANC(j,i)
        nuc = nuc:+anc
      }
      count = count:+nuc
    }
    return count 
  }
  // consensus sequence from aligned sequences
  def consensus_sequence(characters:Vector[Char]):String={
    var consensus:String = ""
    val counts = charCount(characters)
    for (i <- 0 to counts.length-1){
      val I = counts(i).indexOf(counts(i).max)
      val C = characters(I)
      consensus += C
    }
    return consensus
  }
  // alignment matrix 
  def alignment_matrix(characters:Vector[Char]):String={
    var matrix = consensus_sequence(characters)++"\n"
    val counts = charCount(characters)
    val L1 = characters.length-1 // number of characters in alphabet
    val L2 = S(0).length-1 // how long is each sequence
    for (i <- 0 to L1){
      matrix += characters(i).toString++": " // character whose count is being displayed
      for (j <- 0 to L2){
        val C = counts(j)(i).toString // at index j, how many times character characters(i) occurs 
        matrix += C++" "
      }
      matrix += "\n" // next line
    }
    return matrix // returns profile matrix with consensus sequence at the top
  }
}
