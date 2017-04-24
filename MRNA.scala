

class MRNA(peptide:String,codonTable:Vector[String]) {
  // number of ways in which an amino acid can be coded in an RNA triplet
  def AA_Occur(AA:String,codonTable:Vector[String]):BigInt={
    var O:BigInt=0 // number of occurences
    for (i <- 0 to codonTable.length-1){
      val aa=codonTable(i)
      if (aa==AA){
        O+=1
      }
    }
    return O
  }
  // based on the codon table, determines the number of stop codons 
  def stopCodon(codonTable:Vector[String]):BigInt={
    var stop:BigInt = 0
    val L = codonTable.length-1
    for (i <- 0 to L){
      val I = codonTable(i)
      if (I == "Stop"){
        stop += 1
      }
    }
    return stop
  }
  val M = new math()
  // given a peptide sequence, the number of possible RNA strands that can translate it 
  def no_RNA(peptide:String,codonTable:Vector[String]):BigInt={
    var codon1:Vector[String] = Vector()
    var codon2:Vector[BigInt] = Vector()
    var RNAc:BigInt = stopCodon(codonTable) // number of RNA combinations 
    val L = peptide.length-1
    for (i <- 0 to L){
      val AA = peptide(i).toString
      if (codon1.contains(AA) == false){
        codon1 = codon1 :+ AA
        val C = AA_Occur(AA,codonTable)// codons for the amino acid 
        codon2 = codon2 :+ C
      }
      if (codon1.contains(AA) == true){
        val I = codon1.indexOf(AA)
        val z:BigInt = codon2(I)
        RNAc = M.product(RNAc,z)
      }
    }
    return RNAc 
  }
  // returns RNA combinations in Rosalind specified format 
  def no_RNA_format(peptide:String,codonTable:Vector[String]):BigInt={
    return no_RNA(peptide,codonTable)%1000000
  }
  
}
